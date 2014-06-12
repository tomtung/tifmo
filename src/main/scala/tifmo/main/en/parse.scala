package tifmo.main.en

import edu.stanford.nlp.dcoref.CorefChain
import edu.stanford.nlp.dcoref.Dictionaries.MentionType
import tifmo.dcstree._
import tifmo.document._

import java.util.Properties

import scala.collection.JavaConversions._
import scala.annotation.tailrec

import mylib.res.en.EnWordNet

import edu.stanford.nlp.pipeline.{ StanfordCoreNLP, Annotation }
import edu.stanford.nlp.ling.IndexedWord
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.{ CollapsedDependenciesAnnotation, BasicDependenciesAnnotation }
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.{ CorefChainAnnotation, CorefClusterIdAnnotation }

object parse extends ((String, String) => (Document, Document)) {

  private[this] val props = new Properties
  props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
  private[this] val pipeline = new StanfordCoreNLP(props)

  def apply(text: String, hypo: String) = {

    val annotext = new Annotation(text)
    pipeline.annotate(annotext)

    val doctext = makeDocument(annotext, "text")

    addCoreferences(annotext, doctext)

    makeDCSTrees(annotext, doctext)

    tentRootNeg(doctext)

    ///////////

    val annohypo = new Annotation(hypo)
    pipeline.annotate(annohypo)

    val dochypo = makeDocument(annohypo, "hypo")

    makeDCSTrees(annohypo, dochypo)

    tentRootNeg(dochypo)

    //////////////

    val rolesDic = tentRoles(Set(doctext, dochypo))

    tentRoleOrder(rolesDic, doctext)

    tentRoleOrder(rolesDic, dochypo)

    /////////////////

    (doctext, dochypo)
  }

  private[this] def makeDocument(anno: Annotation, id: String) = {

    val tokens = for {
      sentence <- anno.get(classOf[SentencesAnnotation])
      atoken <- sentence.get(classOf[TokensAnnotation])
    } yield {

      val ret = new Token(atoken.get(classOf[TextAnnotation]))

      val pos = atoken.get(classOf[PartOfSpeechAnnotation])
      val ner = atoken.get(classOf[NamedEntityTagAnnotation])
      val lemma = if (ner == "DATE" || ner == "TIME") {
        atoken.get(classOf[NormalizedNamedEntityTagAnnotation])
      } else {
        atoken.get(classOf[LemmaAnnotation])
      }
      val mypos = if (ner == "DATE" || ner == "TIME") {
        "D"
      } else if (pos.matches("JJ.*")) {
        "J"
      } else if (pos.matches("NN.*")) {
        "N"
      } else if (pos.matches("RB.*")) {
        "R"
      } else if (pos.matches("VB.*")) {
        "V"
      } else {
        "O"
      }
      // work around for "each"
      val nner = if (lemma == "each") "O" else ner

      // Treat named-entities that are labeled as singular nouns as singletons by default
      ret.word = EnWord(lemma, mypos, nner, nner != "O" && (pos == "NN" || pos == "NNP"))

      ret
    }

    new Document(id, tokens.toIndexedSeq)
  }

  private[this] def makeDCSTrees(anno: Annotation, doc: Document) {

    case class TokenPos(token: Token, pos: String) {
      def word = token.getWord.asInstanceOf[EnWord]
    }
    case class EdgeInfo(parentToken: TokenPos, relation: String, relationSpecific: String, childToken: TokenPos)

    val docPos = anno.get(classOf[TokensAnnotation]).iterator.map(_.get(classOf[PartOfSpeechAnnotation])).toIndexedSeq

    var counter = 0
    for (sentence <- anno.get(classOf[SentencesAnnotation])) {

      def getTokenPos(x: IndexedWord) = {
        val xid = counter + x.get(classOf[IndexAnnotation]) - 1
        val token = doc.tokens(xid)
        val pos = x.get(classOf[PartOfSpeechAnnotation])
        TokenPos(token, pos)
      }

      var edges = (for (e <- sentence.get(classOf[CollapsedDependenciesAnnotation]).edgeIterable) yield {
        val ptk = getTokenPos(e.getGovernor)
        val rel = e.getRelation.getShortName
        val spc = e.getRelation.getSpecific
        val ctk = getTokenPos(e.getDependent)
        EdgeInfo(ptk, rel, spc, ctk)
      }).toSet

      // Maps prep lemma and token of pobj to prep token
      val prepMap = {
        val semGraph = sentence.get(classOf[BasicDependenciesAnnotation])
        val builder = Map.newBuilder[(String, TokenPos), TokenPos]

        for (
          pobjE <- semGraph.edgeIterable if pobjE.getRelation.getShortName == "pobj"
        ) {
          val prepTokenPos = getTokenPos(pobjE.getGovernor)
          val pobjTokenInfo = getTokenPos(pobjE.getDependent)

          builder +=
            (prepTokenPos.word.lemma, pobjTokenInfo) -> prepTokenPos

          // TODO Should handle conjunction (e.g. as the code commented out as follows),
          // TODO but it breaks the tree structure and causes error
          //					for (
          //						conjE <- semGraph.outgoingEdgeIterable(pobjE.getGovernor)
          //						if conjE.getRelation.getShortName == "conj";
          //						conjDepTokenPos = getTokenPos(conjE.getDependent)
          //						if conjDepTokenPos.pos == "IN"
          //					) {
          //						builder +=
          //							(conjDepTokenPos.word.lemma, pobjTokenInfo) -> conjDepTokenPos
          //					}
        }

        builder.result()
      }

      def edgeInfoToPrepTokenPos(e: EdgeInfo) = {
        if (e.relation == "prep") {
          prepMap.get((e.relationSpecific, e.childToken))
        } else if (e.relation == "psubj") {
          prepMap.get((e.relationSpecific, e.parentToken))
        } else {
          None
        }
      }

      // Helper function: Maps a "RB.*" POS tag to a corresponding "JJ.*" POS tag
      def correctRbPosToJj(pos: String) = pos match {
        case "RB" => "JJ"
        case "RBR" => "JJR"
        case "RBS" => "JJS"
        case _ => pos
      }

      // Helper function: replaces all appearances of one TokenInfo into another among all edges
      def replaceTokenInfo(edges: Set[EdgeInfo])(tokenInfo: TokenPos, newTokenInfo: TokenPos) = edges.map({
        case e @ EdgeInfo(`tokenInfo`, _, _, _) =>
          assert(e.childToken != tokenInfo)
          e.copy(parentToken = newTokenInfo)
        case e @ EdgeInfo(_, _, _, `tokenInfo`) =>
          assert(e.parentToken != tokenInfo)
          e.copy(childToken = newTokenInfo)
        case e =>
          e
      })

      // Ad-hoc fixes of parsing errors (reflected in "dep" relations)
      edges = {
        var ret = edges
        for (depEdge @ EdgeInfo(depP, "dep", _, depCh) <- ret) {
          if (ret.exists(e => e.parentToken == depCh && e.relation == "nsubj" && e.childToken.pos.startsWith("W")) &&
            Set("J", "V").contains(depP.word.mypos)) {
            for (EdgeInfo(`depP`, "nsubj", _, nsubj) <- ret) {
              ret = ret - depEdge + EdgeInfo(nsubj, "rcmod", null, depCh)
            }
          }
        }
        ret
      }

      // This hack is for working around a bug in Stanford CoreNLP
      // In sentences like "Tom is fast", "fast" is incorrectly recognized as an adverb
      // Note: this fix also introduces some errors (albeit benign).
      // For example, in sentence "Tom is here", the adverb "here" would be incorrectly relabeled as an adj.
      // Since this doesn't seem to cause any issue, we live with it for the moment.
      {
        // See whether a word lemma could be an adjective in some context, according to the WordNet
        def canBeAdj(lemma: String) = EnWordNet.synsets(lemma, null).exists(s => s.getType == 3 || s.getType == 5)

        var ret = edges
        for (
          beTokenInfo @ TokenPos(beToken, pos) <- edges.iterator.map(_.parentToken) if beTokenInfo.word.lemma == "be" && pos.matches("VB.*")
        ) {
          // Find the incorrect edges
          // The assumption here is that a "be" verb can't be modified by a adverb w that comes after it,
          // when w could be used as an adjective as well.
          val wrongAdvmodEdges = edges.collect {
            case e @ EdgeInfo(`beTokenInfo`, "advmod", _, childTokenInfo) if canBeAdj(childTokenInfo.word.lemma) &&
              childTokenInfo.token.id > beToken.id => e
          }

          if (wrongAdvmodEdges.nonEmpty) {
            // When there are multiple such edges, choose the last one to revert.
            // This choice is to handle cases similar to:
            //    Tom is definitely fast.
            // in which case both "definitely" and "fast" are label as advmod for "is"
            val edgeToRevert @ EdgeInfo(`beTokenInfo`, "advmod", _, wrongAdjTokenInfo) =
              wrongAdvmodEdges.maxBy(_.childToken.token.id)
            ret -= edgeToRevert

            // Relabel the token as an adjective
            wrongAdjTokenInfo.token.word = wrongAdjTokenInfo.word.copy(mypos = "J")
            val adjTokenInfo = wrongAdjTokenInfo.copy(
              pos = correctRbPosToJj(wrongAdjTokenInfo.pos)
            )

            ret = replaceTokenInfo(ret)(wrongAdjTokenInfo, adjTokenInfo)

            // Also replace all occurences of the be token with our adj
            ret = replaceTokenInfo(ret)(beTokenInfo, adjTokenInfo)

            // Revert the edge and relabel it as "cop"
            // (This is intentionally done after the previous step)
            ret += EdgeInfo(adjTokenInfo, "cop", null, beTokenInfo)

            // Finally update edges after each iteration
            edges = ret
          }
        }
      }

      // This hack is for working around a(nother) bug in Stanford CoreNLP on recognizing adjectival complement
      // For example, in sentences
      //    Neither leading tenor comes cheap.
      //    Pavarotti is a leading tenor who comes cheap.
      // The word "cheap" is incorrectly recognized as an adverb or noun, even it couldn't serve such a role
      // according to the dictionary
      {
        def canOnlyBeAdj(lemma: String) = {
          val synsets = EnWordNet.synsets(lemma, null)
          !synsets.isEmpty && synsets.forall(s => s.getType == 3 || s.getType == 5)
        }

        @tailrec
        def fixEdges(edges: Set[EdgeInfo]): Set[EdgeInfo] = {
          val wrongEdgeOp = edges.find({
            case EdgeInfo(verbTokenInfo, rel, _, childTokenInfo) =>
              rel != "acomp" &&
                verbTokenInfo.pos.matches("VB.*") &&
                childTokenInfo.token.id > verbTokenInfo.token.id &&
                childTokenInfo.word.ner == "O" &&
                !childTokenInfo.pos.startsWith("JJ") &&
                canOnlyBeAdj(childTokenInfo.word.lemma)
          })

          if (wrongEdgeOp.isEmpty) {
            edges
          } else {
            val wrongEdge @ EdgeInfo(verbTokenInfo, _, _, childTokenInfo) = wrongEdgeOp.get
            childTokenInfo.token.word = childTokenInfo.word.copy(mypos = "J")
            val correctChildTokenInfo = childTokenInfo.copy(
              pos =
                if (childTokenInfo.pos.startsWith("RB")) {
                  correctRbPosToJj(childTokenInfo.pos)
                } else {
                  "JJ"
                }
            )

            var updatedEdges = edges - wrongEdge + EdgeInfo(verbTokenInfo, "acomp", null, correctChildTokenInfo)
            updatedEdges = replaceTokenInfo(updatedEdges)(childTokenInfo, correctChildTokenInfo)

            fixEdges(updatedEdges)
          }
        }

        edges = fixEdges(edges)
      }

      // Recognize quantifier "at most" and "at least" + cardinal number
      // As a result, "at_most" and "at_least" will be stored in the "relationSpecific" field of the "num" edge
      edges = {
        var ret = edges

        for (
          numEdge @ EdgeInfo(_, "num", null, numToken) <- edges;
          qmodEdge @ EdgeInfo(`numToken`, "quantmod", null, atToken) <- edges if atToken.word.lemma == "at";
          mweEdge @ EdgeInfo(`atToken`, "mwe", null, mostOrLeastToken) <- edges if Set("most", "least").contains(mostOrLeastToken.word.lemma)
        ) {
          ret = ret - numEdge - qmodEdge - mweEdge +
            numEdge.copy(relationSpecific = "at_" + mostOrLeastToken.word.lemma)
        }

        ret
      }

      // Drop "at least" in other contexts because the system cannot handle them properly.
      // This is just a temporary ad hoc workaround.
      edges = {
        var ret = edges

        for (
          advmodEdge @ EdgeInfo(_, "advmod", null, atToken) <- edges if atToken.word.lemma == "at";
          pobjEdge @ EdgeInfo(`atToken`, "pobj", null, leastToken) <- edges if leastToken.word.lemma == "least"
        ) {
          ret = ret - advmodEdge - pobjEdge
        }

        ret
      }

      // Remove existential "there" when the noun is followed by a prepositional phrase
      edges = {
        var ret = edges

        for (
          explEdge @ EdgeInfo(beToken, "expl", null, thereToken) <- edges if beToken.word.lemma == "be" && thereToken.word.lemma == "there";
          EdgeInfo(`beToken`, "nsubj", null, nounToken) <- edges if nounToken.word.mypos == "N";
          prepEdge @ EdgeInfo(`nounToken`, "prep", _, _) <- edges
        ) {
          ret = ret - explEdge - prepEdge + prepEdge.copy(parentToken = beToken)
        }

        ret
      }

      // copula
      edges = {
        var ret = edges
        for (
          beToken @ TokenPos(_, pos) <- edges.iterator.map(_.parentToken) if beToken.word.lemma == "be" && pos.matches("VB.*")
        ) {
          val compEdgeSet = edges.filter(e => e.parentToken == beToken && e.relation.matches("[cx]?comp"))
          if (compEdgeSet.size == 1) {
            val compEdge @ EdgeInfo(_, _, _, ccomp) = compEdgeSet.head
            ret -= compEdge
            for (edgeFromBe @ EdgeInfo(`beToken`, rrel, _, _) <- edges if !rrel.matches("[cx]?comp")) {
              ret = ret - edgeFromBe +
                edgeFromBe.copy(
                  parentToken = ccomp,
                  relation = if (rrel.matches("[nc]?subj")) "copula" else rrel
                )
            }
            for (edgeToBe @ EdgeInfo(_, _, _, `beToken`) <- edges) {
              ret = ret - edgeToBe + edgeToBe.copy(childToken = ccomp)
            }
          } else {
            val subjEdgeSet = edges.filter(e => e.parentToken == beToken && e.relation.matches("[nc]?subj"))
            if (subjEdgeSet.size == 1) {
              val subjEdge @ EdgeInfo(_, _, _, subj) = subjEdgeSet.head
              ret = ret - subjEdge

              val otherEdgesFromBe = edges.filter(e => e.parentToken == beToken && e != subjEdge)
              val edgesToBe = edges.filter(_.childToken == beToken)

              if (edgesToBe.isEmpty && // The "be" token is the root of sentence
                otherEdgesFromBe.size == 1 && // There's only one other edge from "be"
                otherEdgesFromBe.head.relation == "prep" && // The other edge from "be" is a preposition
                otherEdgesFromBe.head.relationSpecific != null &&
                subj.word.mypos == "N" && otherEdgesFromBe.head.childToken.word.mypos == "N" // The preposition is about relation between nouns
                ) {
                // In such cases, we need to put the child of "prep" edge at the root
                // to enable quantifier ALL and NO on the subject token.
                // Here we introduce a new dependency type "psubj" to denote this.
                val prepEdge @ EdgeInfo(_, _, prepName, prepChild) = otherEdgesFromBe.head
                ret = ret - prepEdge + EdgeInfo(prepChild, "psubj", prepName, subj)
              } else {
                for (edgeFromBe <- otherEdgesFromBe) {
                  ret = ret - edgeFromBe + edgeFromBe.copy(parentToken = subj)
                }
                for (edgeToBe <- edgesToBe) {
                  ret = ret - edgeToBe + edgeToBe.copy(childToken = subj)
                }
              }

            }
          }
        }
        ret
      }

      // Introduce relation "rel:partial-order" for comparative adjectives
      // in sentences like "Jerry is smaller than Tom."
      edges = {
        var ret = edges

        for (
          jjrToken <- edges.map(_.parentToken).filter(_.pos == "JJR");
          edgesFromJjr = edges.filter(_.parentToken == jjrToken) if !edgesFromJjr.exists(_.relation == "neg"); // JJR shouldn't be negated
          // Assuming there's only one of each such nsubjEdge/thanEdge/copEdge
          nsubjEdge @ EdgeInfo(`jjrToken`, "nsubj", null, nsubj) <- edgesFromJjr;
          thanEdge @ EdgeInfo(`jjrToken`, "prep", "than", thanDependent) <- edgesFromJjr;
          copEdge @ EdgeInfo(`jjrToken`, "cop", null, _) <- edgesFromJjr
        ) {
          ret = ret - nsubjEdge - thanEdge - copEdge

          // TODO extract magic string "rel:partial-order" when more relations are added
          ret +=
            EdgeInfo(
              nsubj,
              "rel:partial-order",
              EnWordNet.stem(jjrToken.word.lemma, jjrToken.word.mypos),
              thanDependent
            )

          for (incomingEdge @ EdgeInfo(_, _, _, `jjrToken`) <- edges) {
            ret = ret - incomingEdge + incomingEdge.copy(childToken = nsubj)
          }
        }

        ret
      }

      // rcmod
      edges = {
        var ret = edges
        val sensitive = Set("nsubj", "dobj", "iobj", "nsubjpass")

        for (
          rcmodEdge @ EdgeInfo(ptk, "rcmod", _, ctk) <- edges;
          followingEdge @ EdgeInfo(`ctk`, rrel, _, cctk) <- edges
        ) {
          if (cctk.word.lemma == "when" ||
            (cctk.pos.matches("W.+") && sensitive(rrel))) {
            ret = ret - followingEdge - rcmodEdge +
              rcmodEdge.copy(relationSpecific = if (sensitive(rrel)) rrel else "when")
          } else if (ctk.pos.startsWith("W") && rrel == "prep") {
            // For setences like "People who are from China...", directly collapse to "People from China..."
            ret = ret - followingEdge - rcmodEdge + followingEdge.copy(parentToken = ptk)
          }
        }

        ret
      }

      // some/most/all/each/none/<number> of
      edges = {
        var ret = edges
        for (ofEdge @ EdgeInfo(ptk, "prep", "of", ctk) <- edges) {
          val (doCollapse, reverseRelationOp) =
            ptk.word.lemma match {
              case "some" | "one" =>
                (true, None)
              case "most" | "lot" | "many" | "few" =>
                (true, Some("amod"))
              case "all" | "each" | "none" =>
                (true, Some("det"))
              case lemma if lemma.matches("-?[0-9\\.]+") =>
                (true, Some("num"))
              case _ =>
                (false, None)
            }

          if (doCollapse) {
            ret -= ofEdge
            for (revRel <- reverseRelationOp) {
              ret += EdgeInfo(ctk, revRel, null, ptk)
            }
            for (precedingEdge @ EdgeInfo(_, _, _, `ptk`) <- edges) {
              ret = ret - precedingEdge + precedingEdge.copy(childToken = ctk)
            }
          }
        }
        ret
      }

      // If an adjective is modified by a determiner but doesn't modify a noun,
      // recognize it as an nound instead
      edges = {
        var ret = edges
        for (EdgeInfo(adjTokenPos @ TokenPos(_, "JJ"), "det", _, TokenPos(_, "DT")) <- ret) {
          if (ret.find(e => e.childToken == adjTokenPos && e.relation == "amod").isEmpty) {
            val updatedAdjTokenPos = {
              adjTokenPos.token.word = adjTokenPos.token.word.asInstanceOf[EnWord].copy(mypos = "N")
              adjTokenPos.copy(pos =
                if (adjTokenPos.word.isNamedEntity) {
                  "NNP"
                } else {
                  "NN"
                }
              )
            }
            replaceTokenInfo(ret)(adjTokenPos, updatedAdjTokenPos)
          }
        }
        ret
      }

      // most JJ
      edges = {
        var ret = edges
        for (e @ EdgeInfo(ptk @ TokenPos(_, "JJ"), _, _, ctk) <- edges; if ctk.word.lemma == "most") {
          ret = ret - e + e.copy(parentToken = ptk.copy(pos = "JJS"))
        }
        ret
      }

      // If a named-entity is modified by a determiner other than "the",
      // label it as non-singleton
      {
        for (
          EdgeInfo(TokenPos(neToken, _), "det", _, TokenPos(dtToken, "DT")) <- edges if neToken.word.isSingleton && neToken.word.isNamedEntity && dtToken.word.asInstanceOf[EnWord].lemma != "the"
        ) {
          neToken.word = neToken.word.asInstanceOf[EnWord].copy(isSingleton = false)
        }
      }

      // cluster named entity tokens into chunks
      edges = {
        var ret = edges
        val finish = counter + sentence.get(classOf[TokensAnnotation]).size

        @tailrec
        def mergeNeChunks(i: Int) {
          if (i < finish) {
            val headWord = doc.tokens(i).word.asInstanceOf[EnWord]
            if (headWord.ner == "O") {
              mergeNeChunks(i + 1)
            } else {

              val neRightBound = {

                val sameNeAsHead: Token => Boolean = {
                  val maxBound = (i until finish).takeWhile(j => {
                    val tk = doc.tokens(j)
                    tk.surface.length > 0 && (
                      !tk.surface.head.isLower ||
                      Set("DT", "CC", "IN").contains(docPos(tk.id))
                    )
                  }).lastOption.getOrElse(i)
                  def impl(tk: Token) = {
                    val tkword = tk.word.asInstanceOf[EnWord]
                    tkword.ner == headWord.ner && tk.id <= maxBound && (headWord.mypos != "D" || tkword.lemma == headWord.lemma)
                  }
                  impl
                }

                @tailrec
                def boundRight(prevBound: Int): Int = {
                  var newRightBound = prevBound

                  def updateNewBound(tk1: Token, tk2: Token) {
                    if (tk1.id >= i && tk1.id <= prevBound && sameNeAsHead(tk2)) {
                      newRightBound = math.max(newRightBound, tk2.id)
                    }
                  }

                  for (EdgeInfo(ptk, rel, spc, ctk) <- ret; if rel != "conj" && rel != "prep" && !rel.matches("rel:.*")) {
                    updateNewBound(ptk.token, ctk.token)
                    updateNewBound(ctk.token, ptk.token)
                  }

                  if (newRightBound != prevBound) {
                    boundRight(newRightBound)
                  } else {
                    prevBound
                  }
                }

                boundRight(i)
              }

              assert(neRightBound >= i)

              if (neRightBound == i) {
                mergeNeChunks(i + 1)
              } else {
                val nword =
                  if (headWord.mypos == "D") {
                    headWord
                  } else {
                    val nlemma = (i to neRightBound).map(j => doc.tokens(j).surface).mkString(" ")
                    // For simplicity, label all multi-word named-entities as singletons
                    EnWord(nlemma, "N", headWord.ner, isSingleton = true)
                  }
                doc.tokens(i).word = nword

                (i to neRightBound).find(j => doc.tokens(j).corefID != null) match {
                  case Some(j) => doc.tokens(i).corefID = doc.tokens(j).corefID
                  case None => // Do nothing
                }

                for (e @ EdgeInfo(TokenPos(pToken, pPos), _, _, TokenPos(cToken, cPos)) <- ret.toList) {
                  if (pToken.id >= i && pToken.id <= neRightBound) {
                    ret -= e
                    if (!(cToken.id >= i && cToken.id <= neRightBound)) {
                      ret = ret + e.copy(parentToken = TokenPos(doc.tokens(i), pPos))
                    }
                  }
                  if (cToken.id >= i && cToken.id <= neRightBound) {
                    ret -= e
                    if (!(pToken.id >= i && pToken.id <= neRightBound)) {
                      ret = ret + e.copy(childToken = TokenPos(doc.tokens(i), cPos))
                    }
                  }
                }
                mergeNeChunks(neRightBound + 1)
              }
            }
          }
        }
        mergeNeChunks(counter)
        ret
      }

      // annotate
      for (edge @ EdgeInfo(ptk, rel, spc, ctk) <- edges) {

        val pNode = doc.getTokenNode(ptk.token)
        val cNode = doc.getTokenNode(ctk.token)

        rel match {
          case "copula" =>
            cNode.outRole = ARG
            pNode.addChild(ARG, cNode)

          case "dep" =>
            if (ptk.word.mypos != "O" && ctk.word.mypos != "O") {
              cNode.outRole = ARG
              pNode.addChild(ARG, cNode)
            }

          case "agent" =>
            cNode.outRole = ARG
            pNode.addChild(SBJ, cNode)

          case "acomp" =>
            cNode.outRole = ARG
            pNode.addChild(MOD, cNode)

          case "ccomp" =>
            if (ptk.word.mypos == "V") {
              cNode.outRole = ARG
              pNode.addChild(OBJ, cNode)
            } else if (ptk.word.mypos == "J") {
              cNode.outRole = MOD
              pNode.addChild(ARG, cNode)
            } else {
              cNode.outRole = ARG
              pNode.addChild(ARG, cNode)
            }

          case "xcomp" =>
            cNode.outRole = ARG
            pNode.addChild(ARG, cNode)

          case "dobj" =>
            cNode.outRole = ARG
            pNode.addChild(OBJ, cNode)

          case "iobj" =>
            cNode.outRole = ARG
            pNode.addChild(IOBJ, cNode)

          case "pobj" =>
            cNode.outRole = ARG
            pNode.addChild(IOBJ, cNode)

          case "nsubj" =>
            if (ptk.word.mypos == "V") {
              cNode.outRole = ARG
              pNode.addChild(SBJ, cNode)
            } else if (ptk.word.mypos == "J") {
              cNode.outRole = MOD
              pNode.addChild(ARG, cNode)
            } else {
              cNode.outRole = ARG
              pNode.addChild(ARG, cNode)
            }

          case "nsubjpass" =>
            cNode.outRole = ARG
            pNode.addChild(OBJ, cNode)

          case "csubj" =>
            if (ptk.word.mypos == "V") {
              cNode.outRole = ARG
              pNode.addChild(SBJ, cNode)
            } else if (ptk.word.mypos == "J") {
              cNode.outRole = MOD
              pNode.addChild(ARG, cNode)
            } else {
              cNode.outRole = ARG
              pNode.addChild(ARG, cNode)
            }

          case "csubjpass" =>
            cNode.outRole = ARG
            pNode.addChild(OBJ, cNode)

          case "conj" =>
            pNode.addConjunction(cNode)

          case "amod" =>
            if (ptk.word.mypos == "N" && ctk.pos == "JJS") {
              if (ctk.word.lemma == "most") {
                // pNode.relation = RelQuantifierMost
                pNode.quantifier = QuantifierALL
                pNode.selection = SelQuantifierMost
              } else {
                pNode.selection = SelSup(EnWordNet.stem(ctk.word.lemma, ctk.word.mypos), ARG)
              }
            } else if (ptk.word.mypos == "N" && Set("many", "lot").contains(ctk.word.lemma)) {
              // pNode.relation = RelQuantifierLotOf
              pNode.quantifier = QuantifierALL
              pNode.selection = SelQuantifierLotOf
            } else if (ptk.word.mypos == "N" && ctk.word.lemma == "few") {
              val isAFew = edges.exists(e => e.parentToken == ptk && e.relation == "det" && e.childToken.word.lemma == "a")
              if (isAFew) {
                // pNode.relation = RelQuantifierAFew
                pNode.quantifier = QuantifierALL
                pNode.selection = SelQuantifierAFew
              } else {
                pNode.relation = RelQuantifierFew
                // pNode.quantifier = QuantifierALL
                // pNode.selection = SelQuantifierFew
              }
            } else {
              cNode.outRole = ARG
              pNode.addChild(MOD, cNode)
            }

          case "appos" =>
            cNode.outRole = ARG
            pNode.addChild(ARG, cNode)

          case "advcl" =>
            cNode.outRole = ARG
            pNode.addChild(ARG, cNode)

          case "det" =>
            if (ctk.word.lemma == "all" || ctk.word.lemma == "every" || ctk.word.lemma == "each") {
              pNode.quantifier = QuantifierALL
            } else if (ctk.word.lemma == "no" || ctk.word.lemma == "none" || ctk.word.lemma == "neither") {
              pNode.quantifier = QuantifierNO
            }

          case "predet" =>
            if (ctk.word.lemma == "all") {
              pNode.quantifier = QuantifierALL
            }

          case "infmod" =>
            cNode.outRole = OBJ
            pNode.addChild(ARG, cNode)

          case "partmod" =>
            cNode.outRole = if (ctk.pos == "VBG") SBJ else OBJ
            val tmp = if (ptk.word.mypos == "V") {
              if (edges.exists(x => x.parentToken == ptk && (x.relation == "nsubjpass" || x.relation == "csubjpass"))) OBJ else SBJ
            } else {
              ARG
            }
            pNode.addChild(tmp, cNode)

          case "advmod" =>
            cNode.outRole = ARG
            pNode.addChild(MOD, cNode)

          case "neg" =>
            pNode.sign = false

          case "rcmod" =>
            if (ctk.word.mypos == "V") {
              val tmpmap = Map("when" -> TIME, "nsubj" -> SBJ, "dobj" -> OBJ, "iobj" -> IOBJ, "nsubjpass" -> OBJ)
              cNode.outRole = if (spc == null) ARG else tmpmap(spc)
              val tmp = if (spc == "when") TIME else ARG
              pNode.addChild(tmp, cNode)
            } else if (ctk.word.mypos == "J") {
              cNode.outRole = ARG
              pNode.addChild(MOD, cNode)
            } else {
              cNode.outRole = ARG
              pNode.addChild(ARG, cNode)
            }

          case "nn" =>
            cNode.outRole = ARG
            val tmp = if (ctk.word.mypos == "D" && ptk.word.mypos != "D") {
              TIME
            } else if (ptk.word.isNamedEntity && ctk.word.isNamedEntity) {
              MOD
            } else {
              ARG
            }
            pNode.addChild(tmp, cNode)

          case "npadvmod" =>
            cNode.outRole = ARG
            pNode.addChild(ARG, cNode)

          case "tmod" =>
            cNode.outRole = ARG
            pNode.addChild(TIME, cNode)

          case "num" =>
            cNode.outRole = ARG
            if (ctk.word.mypos == "D") {
              pNode.addChild(TIME, cNode)
            } else if (ptk.word.mypos == "N") {
              spc match {
                case "at_most" =>
                  pNode.relation = RelQuantifierAtMost(ctk.word.lemma)
                // pNode.quantifier = QuantifierALL
                // pNode.selection = SelQuantifierAtMost(ctk.word.lemma)
                case "at_least" =>
                  // pNode.relation = RelQuantifierAtLeast(ctk.word.lemma)
                  pNode.quantifier = QuantifierALL
                  pNode.selection = SelQuantifierAtLeast(ctk.word.lemma)
                case _ =>
                  pNode.selection = SelNum(ctk.word.lemma, ARG)
              }
            } else if (ctk.word.lemma.matches("-?[0-9\\.%]+")) {
              cNode.selection = SelNum(ctk.word.lemma, ARG)
              pNode.addChild(ARG, cNode)
            }

          case "number" =>
            if (ctk.word.lemma.matches("-?[0-9\\.%]+") && ptk.word.mypos == "N") {
              pNode.selection = SelNum(ctk.word.lemma, ARG)
            } else {
              cNode.outRole = ARG
              pNode.addChild(ARG, cNode)
            }

          case "prep" =>
            // TODO Should add more rules to recognize relations like before, after, since, from, to, ...
            // NOTE Changing code for case "prep" might necessitate a similar change in case "psubj"
            if (ctk.word.mypos == "D") {
              cNode.outRole = ARG
              pNode.addChild(TIME, cNode)
            } else if (ctk.word.mypos == "N" && ptk.word.mypos == "N") {
              if (spc == "as") {
                cNode.outRole = ARG
                pNode.addChild(ARG, cNode)
              } else if (spc == null) {
                cNode.outRole = MOD
                pNode.addChild(ARG, cNode)
              } else {
                for (
                  prepTokenPos <- edgeInfoToPrepTokenPos(edge);
                  prepNode = doc.getTokenNode(prepTokenPos.token)
                ) {
                  prepNode.outRole = SBJ
                  pNode.addChild(MOD, prepNode)

                  cNode.outRole = ARG
                  prepNode.addChild(OBJ, cNode)
                }
              }
            } else {
              cNode.outRole = ARG
              pNode.addChild(IOBJ, cNode)
            }

          case "psubj" =>
            // NOTE Changing code for case "psubj" might necessitate a similar change in case "prep"
            if (ptk.word.mypos == "D") {
              cNode.outRole = TIME
              pNode.addChild(ARG, cNode)
            } else if (ptk.word.mypos == "N" && ctk.word.mypos == "N") {
              if (spc == "as") {
                cNode.outRole = ARG
                pNode.addChild(ARG, cNode)
              } else if (spc == null) {
                cNode.outRole = ARG
                pNode.addChild(MOD, cNode)
              } else {
                for (
                  prepTokenPos <- edgeInfoToPrepTokenPos(edge);
                  prepNode = doc.getTokenNode(prepTokenPos.token)
                ) {
                  prepNode.outRole = OBJ
                  pNode.addChild(ARG, prepNode)

                  cNode.outRole = MOD
                  prepNode.addChild(SBJ, cNode)
                }
              }
            } else {
              cNode.outRole = IOBJ
              pNode.addChild(ARG, cNode)
            }

          case "prepc" =>
            cNode.outRole = ARG
            if (spc == null) {
              pNode.addChild(ARG, cNode)
            } else {
              pNode.addChild(ARG, cNode)
            }

          case "poss" =>
            cNode.outRole = ARG
            pNode.addChild(MOD, cNode)

          case "prt" =>
            ptk.token.word = ptk.token.word.asInstanceOf[EnWord].copy(lemma = ptk.word.lemma + " " + ctk.word.lemma)

          case "parataxis" =>
            cNode.outRole = ARG
            pNode.addChild(ARG, cNode)

          case "vmod" =>
            cNode.outRole = ARG
            pNode.addChild(MOD, cNode)

          case "rel:partial-order" =>
            cNode.outRole = MOD
            pNode.addChild(MOD, cNode)
            cNode.relation = RelPartialOrder(spc)

          case _ => // Do nothing
        }
      }

      counter += sentence.get(classOf[TokensAnnotation]).size
    }

  }

  private[this] def addCoreferences(anno: Annotation, doc: Document) {

    def onlyIndefiniteMention(c: CorefChain): Boolean = {
      c.getMentionsInTextualOrder.iterator.map(_.startIndex).forall(i => {
        val startWord = doc.tokens(i - 1).getWord.asInstanceOf[EnWord]
        startWord.mypos == "O" && Set("a", "an", "some").contains(startWord.lemma)
      })
    }

    val corefMap = anno.get(classOf[CorefChainAnnotation]).toMap

    var counter = 0
    for {
      sentence <- anno.get(classOf[SentencesAnnotation])
      atoken <- sentence.get(classOf[TokensAnnotation])
    } {
      val tmp = atoken.get(classOf[CorefClusterIdAnnotation])
      if (tmp != null && !onlyIndefiniteMention(corefMap(tmp))) {
        doc.tokens(counter).corefID = tmp.toString
      }
      counter += 1
    }
  }
}
