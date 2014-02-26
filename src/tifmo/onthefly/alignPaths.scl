package tifmo

import dcstree.DCSTreeNode
import dcstree.DCSTreeEdgeNormal
import dcstree.Ref
import dcstree.Declarative
import dcstree.DeclarativeSubRef
import inference.IEngine

package onthefly {
	
	object alignPaths extends ((Set[Declarative], IEngine) => Set[PathAlignment]) {
		
		def apply(assumption: Set[Declarative], ie: IEngine) = {
			for {
				DeclarativeSubRef(sub, sup) <- assumption
				if logicallyRelated(ie, sub, sup)
				subPaths = generatePaths(sub)
				supPaths = generatePaths(sup)
				subPath <- subPaths
				supPath <- supPaths
				if subPath.rnrs.length <= 5
				if supPath.rnrs.length <= 5
				if logicallyAligned(ie, subPath, supPath)
				if !headStopWord(subPath)
				if !headNamedEntity(supPath)
			} yield {
				PathAlignment(subPath, supPath)
			}
		}
		
		private[this] def logicallyRelated(ie: IEngine, a: Ref, b: Ref) = {
			val atm = ie.getTerm(a.getDenotation)
			val btm = ie.getTerm(b.getDenotation)
			(atm.superSets intersect btm.superSets).exists(!_.isW) || 
				(!atm.selfDisjoint && !btm.selfDisjoint && 
					(atm.disjointSets intersect btm.disjointSets).exists(!_.selfDisjoint))
		}
		
		private[this] def generatePaths(ref: Ref) = {
			val tmp = ref.subOrdinate + ((Seq.empty[(DCSTreeNode, DCSTreeEdgeNormal)], Seq.empty[(DCSTreeEdgeNormal, DCSTreeNode)], ref))
			for {
				(asc, dec, end) <- tmp
				if asc.isEmpty || dec.isEmpty || (ref.node +: asc.map(_._1)).takeRight(2).head != dec.head._2
				if end.node.selection == null
			} yield {
				new Path(ref, asc, dec, !end.node.children.isEmpty)
			}
		}
		
		private[this] def logicallyAligned(ie: IEngine, subPath: Path, supPath: Path) = {
			if (subPath.soft && supPath.soft) {
				val hatm = ie.getTerm(subPath.start.getDenotation)
				val hbtm = ie.getTerm(supPath.start.getDenotation)
				val commsup = hatm.superSets intersect hbtm.superSets
				val an = subPath.rnrs.last._2
				val bn = supPath.rnrs.last._2
				an.rseq.exists(ar => bn.rseq.exists(br => {
					val atm = ie.getTerm(an.germ(ar))
					val btm = ie.getTerm(bn.germ(br))
					(atm.superSets intersect btm.superSets).exists(!commsup.contains(_))
				}))
			} else {
				!subPath.soft && !supPath.soft
			}
		}
		
		private[this] def headStopWord(path: Path) = {
			path.headWords.forall(_.isStopWord)
		}
		
		private[this] def headNamedEntity(path: Path) = {
			path.headWords.exists(_.isNamedEntity)
		}
		
	}
	
}
