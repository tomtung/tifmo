package tifmo

import dcstree.DCSTreeEdge
import dcstree.DCSTreeEdgeNormal
import dcstree.DCSTreeEdgeQuantifier
import dcstree.QuantifierALL
import dcstree.QuantifierNO
import dcstree.DCSTreeNode
import dcstree.RefOutput
import dcstree.RefGerm
import dcstree.Context
import dcstree.ContextA
import dcstree.ContextB
import dcstree.DeclarativeSubRef
import inference.IEngine

package onthefly {
	
	case class PathAlignment(subPath: Path, supPath: Path) {
		
		def toOnTheFly(ie: IEngine) = {
			
			val ctx = subPath.start.subOrdinate.map(_._3) -- subPath.asc.map(x => RefGerm(x._1, x._2.inRole)) -- subPath.dec.map(x => RefOutput(x._2))
			val qtall = (for ((asc, dec, end) <- subPath.start.subOrdinate; if end.node.selection == null) yield {
				end.quantifyALL
			}).flatten
			val qtno = (for ((asc, dec, end) <- subPath.start.subOrdinate; if end.node.selection == null) yield {
				end.quantifyNO
			}).flatten
			
			val sup = {
				
				val ctxtm = for (ref <- ctx) yield ((ref, ie.getTerm(ref.getDenotation)))
				val alltm = for (ref <- qtall) yield ((ref, ie.getTerm(ref.getDenotation)))
				val notm = for (ref <- qtno) yield ((ref, ie.getTerm(ref.getDenotation)))
				
				def xctxqt(cdash: Set[(DCSTreeEdge, DCSTreeNode)]) = {
					for ((e, n) <- cdash) ie.getTerm(n.output)
					
					val xctx = (for ((DCSTreeEdgeNormal(r), n) <- cdash) yield {
						val tm = ie.getTerm(n.output)
						for ((ref, x) <- ctxtm; if (x.superSets intersect tm.superSets).exists(!_.isW)) yield {
							ContextA(r, ref):Context
						}
					}).flatten
					val xqt = (for ((e:DCSTreeEdgeQuantifier, n) <- cdash) yield {
						val tm = ie.getTerm(n.output)
						val pool = e.quantifier match {
							case QuantifierALL => alltm
							case QuantifierNO => notm
						}
						for ((ref, x) <- pool; if (x.superSets intersect tm.superSets).exists(!_.isW)) yield {
							(e:DCSTreeEdge, ref.node.copy)
						}
					}).flatten
					(xctx, xqt)
				}
				
				def mkDCSTreeNode(cp: DCSTreeNode, seq: Seq[(DCSTreeEdgeNormal, DCSTreeNode)]): Seq[DCSTreeNode] = {
					val cdash = if (seq.isEmpty) cp.children else (cp.children - seq.head)
					val (xctx, xqt) = xctxqt(cdash)
					val (prechild, rettail) = if (seq.isEmpty) {
						(Set.empty[(DCSTreeEdge, DCSTreeNode)], Seq.empty[DCSTreeNode])
					} else {
						val tmp = mkDCSTreeNode(seq.head._2, seq.tail)
						(Set((seq.head._1:DCSTreeEdge, tmp.head)), tmp)
					}
					new DCSTreeNode(
						prechild ++ xqt, 
						cp.rseq, 
						cp.word, 
						cp.sign, 
						cp.selection, 
						cp.outRole, 
						xctx
					) +: rettail
				}
				
				val topref = if (supPath.asc.isEmpty) {
					supPath.start
				} else {
					val (tmpn, tmpe) = supPath.asc.last
					RefGerm(tmpn, tmpe.inRole)
				}
				val pretopctx = if (topref.isInstanceOf[RefGerm] && topref.node.compare2outRole(topref.role) >= 0 && topref.node.parent != null) {
					val (pn, e) = topref.node.parent
					Set(ContextB(RefGerm(pn, e.inRole)):Context)
				} else {
					Set.empty[Context]
				}
				val ascrev = if (supPath.asc.isEmpty) {
					Seq.empty[(DCSTreeEdgeNormal, DCSTreeNode)]
				} else {
					val (tmp1, tmp2) = supPath.asc.reverse.unzip
					(tmp2 zip (supPath.start.node +: tmp1.init)).reverse
				}
				val topcdash = topref.node.children -- supPath.dec.take(1) -- ascrev.take(1)
				val (topctx, topqt) = xctxqt(topcdash)
				val topc1 = if (supPath.dec.isEmpty) {
					Set.empty[(DCSTreeEdge, DCSTreeNode)]
				} else {
					val tmp = mkDCSTreeNode(supPath.dec.head._2, supPath.dec.tail)
					Set((supPath.dec.head._1:DCSTreeEdge, tmp.head))
				}
				val (topc2, lastnode) = if (ascrev.isEmpty) {
					(Set.empty[(DCSTreeEdge, DCSTreeNode)], null:DCSTreeNode)
				} else {
					val tmp = mkDCSTreeNode(ascrev.head._2, ascrev.tail)
					(Set((ascrev.head._1:DCSTreeEdge, tmp.head)), tmp.last)
				}
				val rt = new DCSTreeNode(
					topc1 ++ topc2 ++ topqt, 
					topref.node.rseq, 
					topref.node.word, 
					topref.node.sign, 
					topref.node.selection, 
					topref.node.outRole, 
					topctx ++ pretopctx
				)
				rt.upward()
				rt.downward(null)
				if (ascrev.isEmpty) {
					supPath.start match {
						case RefOutput(n) => RefOutput(rt)
						case RefGerm(n, r) => RefGerm(rt, r)
					}
				} else {
					RefGerm(lastnode, supPath.start.role)
				}
			}
			DeclarativeSubRef(subPath.start, sup)
		}
		
	}
	
}
