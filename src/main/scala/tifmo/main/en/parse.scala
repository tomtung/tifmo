package tifmo

import dcstree.{QuantifierALL, QuantifierNO}
import document.{SelNum, SelSup, Token, Document, tentRootNeg, tentRoles, tentRoleOrder, SelGeneralizedQuantifier}

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
import tifmo.document.{AtLeastQuantifier, AtMostQuantifier, MostQuantifier, RelPartialOrder}
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

			case class TokenPos(token: Token, pos: String) {
				def word = token.getWord.asInstanceOf[EnWord]
			}
			case class EdgeInfo(parentToken: TokenPos, relation: String, relationSpecific: String, childToken: TokenPos)

			var counter = 0
			for (sentence <- anno.get(classOf[SentencesAnnotation])) {

				def tokeninfo(x: IndexedWord) = {
					val xid = counter + x.get(classOf[IndexAnnotation]) - 1
					val token = doc.tokens(xid)
					val pos = x.get(classOf[PartOfSpeechAnnotation])
					TokenPos(token, pos)
				}

				var edges = (for (e <- sentence.get(classOf[CollapsedDependenciesAnnotation]).edgeIterable) yield {
					val ptk = tokeninfo(e.getGovernor)
					val rel = e.getRelation.getShortName
					val spc = e.getRelation.getSpecific
					val ctk = tokeninfo(e.getDependent)
					EdgeInfo(ptk, rel, spc, ctk)
				}).toSet


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
						beTokenInfo @ TokenPos(beToken, pos) <- edges.iterator.map(_.parentToken)
						if beTokenInfo.word.lemma == "be" && pos.matches("VB.*")
					) {
						// Find the incorrect edges
						// The assumption here is that a "be" verb can't be modified by a adverb w that comes after it,
						// when w could be used as an adjective as well.
						val wrongAdvmodEdges = edges.collect {
							case e @ EdgeInfo(`beTokenInfo`, "advmod", _, childTokenInfo)
								if canBeAdj(childTokenInfo.word.lemma) &&
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
									if(childTokenInfo.pos.startsWith("RB")) {
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

				// copula
				edges = {
					var ret = edges
					for (
						beToken @ TokenPos(_, pos) <- edges.iterator.map(_.parentToken)
						if beToken.word.lemma == "be" && pos.matches("VB.*")
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
					   followingEdge @ EdgeInfo(`ctk`, rrel, _, cctk) <- edges
					   if (cctk.pos.matches("W.+") && sensitive(rrel)) || cctk.word.lemma == "when"
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
								case "some" | "one" | "lot" =>
									(true, None)
								case "most"=>
									(true, Some("advmod"))
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
					for (e @ EdgeInfo(ptk @ TokenPos(_, "JJ"), _, _, ctk) <- edges; if ctk.word.lemma == "most") {
						ret = ret - e + e.copy(parentToken = ptk.copy(pos = "JJS"))
					}
					ret
				}

				// Recognize quantifier "at most" and "at least" + cardinal number
				// As a result, "at_most" and "at_least" will be stored in the "relationSpecific" field of the "num" edge
				edges = {
					var ret = edges

					for (
						numEdge @ EdgeInfo(_, "num", null, numToken) <- edges;
						qmodEdge @ EdgeInfo(`numToken`, "quantmod", null, atToken) <- edges
						if atToken.word.lemma == "at";
						mweEdge @ EdgeInfo(`atToken`, "mwe", null, mostOrLeastToken) <- edges
						if Set("most", "least").contains(mostOrLeastToken.word.lemma)
					) {
						ret = ret - numEdge - qmodEdge - mweEdge +
							numEdge.copy(relationSpecific = "at_" + mostOrLeastToken.word.lemma)
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

									for (e @ EdgeInfo(TokenPos(pToken, pPos), _, _, TokenPos(cToken, cPos)) <- ret.toList) {
										if (pToken.id >= i && pToken.id <= tmpmax) {
											ret -= e
											if (!(cToken.id >= i && cToken.id <= tmpmax)) {
												ret = ret + e.copy(parentToken = TokenPos(doc.tokens(i), pPos))
											}
										}
										if (cToken.id >= i && cToken.id <= tmpmax) {
											ret -= e
											if (!(pToken.id >= i && pToken.id <= tmpmax)) {
												ret = ret + e.copy(childToken = TokenPos(doc.tokens(i), cPos))
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
									pNode.selection = SelGeneralizedQuantifier(MostQuantifier)
									pNode.quantifier = QuantifierALL
								} else {
									pNode.selection = SelSup(EnWordNet.stem(ctk.word.lemma, ctk.word.mypos), ARG)
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
										pNode.selection = SelGeneralizedQuantifier(AtMostQuantifier(ctk.word.lemma))
									case "at_least" =>
										pNode.selection = SelGeneralizedQuantifier(AtLeastQuantifier(ctk.word.lemma))
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
							cNode.outRole = ARG
							if (ctk.word.mypos == "D") {
								// we should add more rules to recognize
								 // relations like before, after, since, from, to, ...
								pNode.addChild(TIME, cNode)
							} else if (ctk.word.mypos == "N" && ptk.word.mypos == "N") {
								if (spc == "as") {
									pNode.addChild(ARG, cNode)
								} else if (spc == "of" || spc == "by") {
									pNode.addChild(MOD, cNode)
								} else if (spc == null) {
									cNode.outRole = MOD
									pNode.addChild(ARG, cNode)
								} else {
									pNode.addChild(MOD, cNode)
								}
							} else {
								pNode.addChild(IOBJ, cNode)
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
							val nword = EnWord(ptk.word.lemma + " " + ctk.word.lemma, ptk.word.mypos, ptk.word.ner)
							ptk.token.word = nword

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
