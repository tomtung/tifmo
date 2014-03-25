package tifmo

import dcstree.{QuantifierALL, QuantifierNO}
import document.{SelNum, SelSup, Token, Document, tentRootNeg, tentRoles, tentRoleOrder}

import mylib.res.en.EnWordNet

import edu.stanford.nlp.pipeline.{StanfordCoreNLP, Annotation}
import edu.stanford.nlp.ling.IndexedWord
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.CollapsedDependenciesAnnotation
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefClusterIdAnnotation

import java.util.Properties

import scala.collection.JavaConversions._

package main.en {

import scala.annotation.tailrec
import tifmo.document.RelPartialOrder
import tifmo.dcstree.Relation

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
				ret.word = EnWord(lemma, mypos, nner)

				ret
			}

			new Document(id, tokens.toIndexedSeq)
		}

		private[this] def makeDCSTrees(anno: Annotation, doc: Document) {

			case class TokenInfo(token: Token, word: EnWord, pos: String)
			case class EdgeInfo(parentToken: TokenInfo, relation: String, relationSpecific: String, childToken: TokenInfo)

			var counter = 0
			for (sentence <- anno.get(classOf[SentencesAnnotation])) {

				def tokeninfo(x: IndexedWord) = {
					val xid = counter + x.get(classOf[IndexAnnotation]) - 1
					val token = doc.tokens(xid)
					val word = token.word.asInstanceOf[EnWord]
					val pos = x.get(classOf[PartOfSpeechAnnotation])
					TokenInfo(token, word, pos)
				}

				var edges = (for (e <- sentence.get(classOf[CollapsedDependenciesAnnotation]).edgeIterable) yield {
					val ptk = tokeninfo(e.getGovernor)
					val rel = e.getRelation.getShortName
					val spc = e.getRelation.getSpecific
					val ctk = tokeninfo(e.getDependent)
					EdgeInfo(ptk, rel, spc, ctk)
				}).toSet

				// This hack is for working around a bug in Stanford CoreNLP
				// In sentences like "Tom is fast", "fast" is incorrectly recognized as an adverb
				{
					// See whether a word lemma could be an adjective in some context, according to the WordNet
					def canBeAdj(lemma: String) = EnWordNet.synsets(lemma, null).exists(_.getType == 3)

					// Maps a "RB.*" POS tag to a corresponding "JJ.*" POS tag
					def correctRbPosToJj(pos: String) = pos match {
						case "RB" => "JJ"
						case "RBR" => "JJR"
						case "RBS" => "JJS"
						case _ => pos
					}

					var ret = edges
					for (
						beTokenInfo @ TokenInfo(beToken, beWord, pos) <- edges.iterator.map(_.parentToken)
						if beWord.lemma == "be" && pos.matches("VB.*");
						// Find the incorrect edges
						// The assumption here is that a "be" verb can't be modified by a adverb w that comes after it,
						// when w could be used as an adjective as well.
						wrongAdvmodEdges = edges.collect {
							case e @ EdgeInfo(`beTokenInfo`, "advmod", _, childTokenInfo)
								if canBeAdj(childTokenInfo.word.lemma) &&
									childTokenInfo.token.id > beToken.id => e
						}
						if wrongAdvmodEdges.nonEmpty
					) {
						// When there are multiple such edges, choose the last one to revert.
						// This choice is to handle cases similar to:
						//    Tom is definitely fast.
						// in which case both "definitely" and "fast" are label as advmod for "is"
						val edgeToRevert @ EdgeInfo(`beTokenInfo`, "advmod", _, wrongAdjTokenInfo) =
							wrongAdvmodEdges.maxBy(_.childToken.token.id)
						ret -= edgeToRevert

						// Relabel the token as an adjective
						val adjTokenInfo = TokenInfo(
							token = wrongAdjTokenInfo.token,
							word = wrongAdjTokenInfo.word.copy(mypos = "J"),
							pos = correctRbPosToJj(wrongAdjTokenInfo.pos)
						)

						// Revert the edge and relabel it as "cop"
						ret += EdgeInfo(adjTokenInfo, "cop", null, beTokenInfo)

						// Replace all other appearance of wrongAdjTokenInfo and the "be" verb with adjTokenInfo
						def replaceWithAdjTokenInfo(tokenInfo: TokenInfo) {
							for (e @ EdgeInfo(`tokenInfo`, _, _, _) <- edges if e != edgeToRevert) {
								ret -= e
								ret += e.copy(parentToken = adjTokenInfo)
							}

							for (e @ EdgeInfo(_, _, _, `tokenInfo`) <- edges if e != edgeToRevert) {
								ret -= e
								ret += e.copy(childToken = adjTokenInfo)
							}
						}

						replaceWithAdjTokenInfo(wrongAdjTokenInfo)
						replaceWithAdjTokenInfo(beTokenInfo)

						// Finally update edges after each iteration
						edges = ret
					}
				}

				// copula
				edges = {
					var ret = edges
					for (
						beToken @ TokenInfo(_, wd, pos) <- edges.iterator.map(_.parentToken)
						if wd.lemma == "be" && pos.matches("VB.*")
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
								val subjEdge @ EdgeInfo(_, _, _, csubj) = subjEdgeSet.head
								ret = ret - subjEdge
								for (edgeFromBe @ EdgeInfo(`beToken`, rrel, _, _) <- edges if !rrel.matches("[nc]?subj")) {
									ret = ret - edgeFromBe + edgeFromBe.copy(parentToken = csubj)
								}
								for (edgeToBe @ EdgeInfo(_, _, _, `beToken`) <- edges) {
									ret = ret - edgeToBe + edgeToBe.copy(childToken = csubj)
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

					for(
						jjrToken <- edges.map(_.parentToken).filter(_.pos == "JJR");
						edgesFromJjr = edges.filter(_.parentToken == jjrToken)
						if !edgesFromJjr.exists(_.relation == "neg"); // JJR shouldn't be negated
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
						rcmodEdge @ EdgeInfo(_, "rcmod", _, ctk) <- edges;
					   followingEdge @ EdgeInfo(`ctk`, rrel, _, TokenInfo(_, ccWord, ccPos)) <- edges
					   if (ccPos.matches("W.+") && sensitive(rrel)) || ccWord.lemma == "when"
					) {
						ret = ret - followingEdge - rcmodEdge +
							rcmodEdge.copy(relationSpecific = if (sensitive(rrel)) rrel else "when")
					}
					ret
				}

				// some/most/all/each/none/<number> of
				edges = {
					var ret = edges
					for (ofEdge @ EdgeInfo(ptk, "prep", "of", ctk) <- edges) {
						val (doCollapse, reverseRelationOp) =
							ptk.word.lemma match {
								case "some" | "most" =>
									(true, None)
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

				// most JJ
				edges = {
					var ret = edges
					for (e @ EdgeInfo(ptk @ TokenInfo(_, _, "JJ"), _, _, ctk) <- edges; if ctk.word.lemma == "most") {
						ret = ret - e + e.copy(parentToken = ptk.copy(pos = "JJS"))
					}
					ret
				}

				// cluster named entity tokens into chunks
				edges = {
					var ret = edges
					val finish = counter + sentence.get(classOf[TokensAnnotation]).size

					@tailrec
					def scan(i: Int) {
						if (i < finish) {
							val theword = doc.tokens(i).word.asInstanceOf[EnWord]
							if (theword.ner == "O") {
								scan(i + 1)
							} else {
								val tmpmax = {
									def sameNE(tk: Token) = {
										val tkword = tk.word.asInstanceOf[EnWord]
										tkword.ner == theword.ner && (theword.mypos != "D" || tkword.lemma == theword.lemma)
									}
									var tmp = Set.empty[Int]
									var cachemax = i
									@tailrec
									def loop() {
										for (EdgeInfo(ptk, rel, spc, ctk) <- ret; if rel != "conj" && rel != "prep" && !rel.matches("rel:.*")) {
											if (ptk.token.id >= i && ptk.token.id <= cachemax && sameNE(ctk.token)) {
												tmp += ctk.token.id
											}
											if (ctk.token.id >= i && ctk.token.id <= cachemax && sameNE(ptk.token)) {
												tmp += ptk.token.id
											}
										}

										if (!tmp.isEmpty && tmp.max != cachemax) {
											cachemax = tmp.max
											loop()
										}
									}

									loop()
									cachemax
								}
								assert(tmpmax >= i)
								if (tmpmax == i) {
									scan(i + 1)
								} else {
									val nword =
										if (theword.mypos == "D") {
											theword
										} else {
											val nlemma = (i to tmpmax).map(j => doc.tokens(j).surface).mkString(" ")
											EnWord(nlemma, "N", theword.ner)
										}
									doc.tokens(i).word = nword

									(i to tmpmax).find(j => doc.tokens(j).corefID != null) match {
										case Some(j) => doc.tokens(i).corefID = doc.tokens(j).corefID
										case None => // Do nothing
									}

									for (e @ EdgeInfo(TokenInfo(pToken, _, pPos), _, _, TokenInfo(cToken, _, cPos)) <- ret.toList) {
										if (pToken.id >= i && pToken.id <= tmpmax) {
											ret -= e
											if (!(cToken.id >= i && cToken.id <= tmpmax)) {
												ret = ret + e.copy(parentToken = TokenInfo(doc.tokens(i), nword, pPos))
											}
										}
										if (cToken.id >= i && cToken.id <= tmpmax) {
											ret -= e
											if (!(pToken.id >= i && pToken.id <= tmpmax)) {
												ret = ret + e.copy(childToken = TokenInfo(doc.tokens(i), nword, cPos))
											}
										}
									}
									scan(tmpmax + 1)
								}
							}
						}
					}
					scan(counter)
					ret
				}

				// annotate
				for (EdgeInfo(ptk, rel, spc, ctk) <- edges) {
					rel match {
						case "copula" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))

						case "dep" =>
							if (ptk.word.mypos != "O" && ctk.word.mypos != "O") {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							}

						case "agent" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(SBJ, doc.getTokenNode(ctk.token))

						case "acomp" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(MOD, doc.getTokenNode(ctk.token))

						case "ccomp" =>
							if (ptk.word.mypos == "V") {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(OBJ, doc.getTokenNode(ctk.token))
							} else if (ptk.word.mypos == "J") {
								doc.getTokenNode(ctk.token).outRole = MOD
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							} else {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							}

						case "xcomp" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))

						case "dobj" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(OBJ, doc.getTokenNode(ctk.token))

						case "iobj" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(IOBJ, doc.getTokenNode(ctk.token))

						case "pobj" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(IOBJ, doc.getTokenNode(ctk.token))

						case "nsubj" =>
							if (ptk.word.mypos == "V") {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(SBJ, doc.getTokenNode(ctk.token))
							} else if (ptk.word.mypos == "J") {
								doc.getTokenNode(ctk.token).outRole = MOD
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							} else {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							}

						case "nsubjpass" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(OBJ, doc.getTokenNode(ctk.token))

						case "csubj" =>
							if (ptk.word.mypos == "V") {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(SBJ, doc.getTokenNode(ctk.token))
							} else if (ptk.word.mypos == "J") {
								doc.getTokenNode(ctk.token).outRole = MOD
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							} else {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							}

						case "csubjpass" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(OBJ, doc.getTokenNode(ctk.token))

						case "conj" =>
							doc.getTokenNode(ptk.token).addConjunction(doc.getTokenNode(ctk.token))

						case "amod" =>
							if (ptk.word.mypos == "N" && ctk.pos == "JJS") {
								doc.getTokenNode(ptk.token).selection = SelSup(EnWordNet.stem(ctk.word.lemma, ctk.word.mypos), ARG)
							} else {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(MOD, doc.getTokenNode(ctk.token))
							}

						case "appos" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))

						case "advcl" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))

						case "det" =>
							if (ctk.word.lemma == "all" || ctk.word.lemma == "every" || ctk.word.lemma == "each") {
								doc.getTokenNode(ptk.token).quantifier = QuantifierALL
							} else if (ctk.word.lemma == "no" || ctk.word.lemma == "none") {
								doc.getTokenNode(ptk.token).quantifier = QuantifierNO
							}

						case "predet" =>
							if (ctk.word.lemma == "all") {
								doc.getTokenNode(ptk.token).quantifier = QuantifierALL
							}

						case "infmod" =>
							doc.getTokenNode(ctk.token).outRole = OBJ
							doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))

						case "partmod" =>
							doc.getTokenNode(ctk.token).outRole = if (ctk.pos == "VBG") SBJ else OBJ
							val tmp = if (ptk.word.mypos == "V") {
								if (edges.exists(x => x.parentToken == ptk && (x.relation == "nsubjpass" || x.relation == "csubjpass"))) OBJ else SBJ
							} else {
								ARG
							}
							doc.getTokenNode(ptk.token).addChild(tmp, doc.getTokenNode(ctk.token))

						case "advmod" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(MOD, doc.getTokenNode(ctk.token))

						case "neg" =>
							doc.getTokenNode(ptk.token).sign = false

						case "rcmod" =>
							if (ctk.word.mypos == "V") {
								val tmpmap = Map("when" -> TIME, "nsubj" -> SBJ, "dobj" -> OBJ, "iobj" -> IOBJ, "nsubjpass" -> OBJ)
								doc.getTokenNode(ctk.token).outRole = if (spc == null) ARG else tmpmap(spc)
								val tmp = if (spc == "when") TIME else ARG
								doc.getTokenNode(ptk.token).addChild(tmp, doc.getTokenNode(ctk.token))
							} else if (ctk.word.mypos == "J") {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(MOD, doc.getTokenNode(ctk.token))
							} else {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							}

						case "nn" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							val tmp = if (ctk.word.mypos == "D" && ptk.word.mypos != "D") {
								TIME
							} else if (ptk.word.isNamedEntity && ctk.word.isNamedEntity) {
								MOD
							} else {
								ARG
							}
							doc.getTokenNode(ptk.token).addChild(tmp, doc.getTokenNode(ctk.token))

						case "npadvmod" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))

						case "tmod" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(TIME, doc.getTokenNode(ctk.token))

						case "num" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							if (ctk.word.mypos == "D") {
								doc.getTokenNode(ptk.token).addChild(TIME, doc.getTokenNode(ctk.token))
							} else if (ptk.word.mypos == "N") {
								doc.getTokenNode(ptk.token).selection = SelNum(ctk.word.lemma, ARG)
							} else if (ctk.word.lemma.matches("-?[0-9\\.%]+")) {
								doc.getTokenNode(ctk.token).selection = SelNum(ctk.word.lemma, ARG)
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							}

						case "number" =>
							if (ctk.word.lemma.matches("-?[0-9\\.%]+") && ptk.word.mypos == "N") {
								doc.getTokenNode(ptk.token).selection = SelNum(ctk.word.lemma, ARG)
							} else {
								doc.getTokenNode(ctk.token).outRole = ARG
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							}

						case "prep" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							if (ctk.word.mypos == "D") {
								// we should add more rules to recognize
								 // relations like before, after, since, from, to, ...
								doc.getTokenNode(ptk.token).addChild(TIME, doc.getTokenNode(ctk.token))
							} else if (ctk.word.mypos == "N" && ptk.word.mypos == "N") {
								if (spc == "as") {
									doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
								} else if (spc == "of" || spc == "by") {
									doc.getTokenNode(ptk.token).addChild(MOD, doc.getTokenNode(ctk.token))
								} else if (spc == null) {
									doc.getTokenNode(ctk.token).outRole = MOD
									doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
								} else {
									doc.getTokenNode(ptk.token).addChild(MOD, doc.getTokenNode(ctk.token))
								}
							} else {
								doc.getTokenNode(ptk.token).addChild(IOBJ, doc.getTokenNode(ctk.token))
							}

						case "prepc" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							if (spc == null) {
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							} else {
								doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))
							}

						case "poss" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(MOD, doc.getTokenNode(ctk.token))

						case "prt" =>
							val nword = EnWord(ptk.word.lemma + " " + ctk.word.lemma, ptk.word.mypos, ptk.word.ner)
							ptk.token.word = nword

						case "parataxis" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(ARG, doc.getTokenNode(ctk.token))

						case "vmod" =>
							doc.getTokenNode(ctk.token).outRole = ARG
							doc.getTokenNode(ptk.token).addChild(MOD, doc.getTokenNode(ctk.token))

						case "rel:partial-order" =>
							val ctkNode = doc.getTokenNode(ctk.token)
							ctkNode.outRole = MOD
							doc.getTokenNode(ptk.token).addChild(MOD, ctkNode)
							ctkNode.relation = RelPartialOrder(spc)

						case _ =>
							// Do nothing
					}
				}

				counter += sentence.get(classOf[TokensAnnotation]).size
			}

		}

		private[this] def addCoreferences(anno: Annotation, doc: Document) {

			var counter = 0
			for {
				sentence <- anno.get(classOf[SentencesAnnotation])
				atoken <- sentence.get(classOf[TokensAnnotation])
			} {
				val tmp = atoken.get(classOf[CorefClusterIdAnnotation])
				if (tmp != null) doc.tokens(counter).corefID = tmp.toString
				counter += 1
			}
		}

	}

}
