package tifmo

import dcstree.SemRole
import dcstree.DCSTreeNode
import dcstree.DCSTreeEdgeNormal
import dcstree.Ref

package onthefly {
	/**
	 * Path in a DCS tree. 
	 */
	class Path(
		/**
		 * The germ from which this path begins.
		 */
		val start: Ref, 
		private[onthefly] val asc: Seq[(DCSTreeNode, DCSTreeEdgeNormal)], 
		private[onthefly] val dec: Seq[(DCSTreeEdgeNormal, DCSTreeNode)], 
		/**
		 * `soft == false` if and only if the end point of this path is a leaf.
		 */
		val soft: Boolean
	) extends Serializable {
		/**
		 * Representing the path in a `role(node)role-...-role(node)` manner. 
		 */
		val rnrs = {
			var tmpnode = start.node
			var tmprole = start.role
			var revret = Seq.empty[(SemRole, DCSTreeNode, SemRole)]
			for ((n, e) <- asc) {
				revret = ((tmprole, tmpnode, tmpnode.outRole)) +: revret
				tmpnode = n
				tmprole = e.inRole
			}
			for ((e, n) <- dec) {
				revret = ((tmprole, tmpnode, e.inRole)) +: revret
				tmpnode = n
				tmprole = n.outRole
			}
			revret = ((tmprole, tmpnode, null)) +: revret
			revret.reverse
		}
		
		val headWords = {
			var flag = true
			val tmp = rnrs.takeWhile(x => {
				val ret = flag
				if (x._1 != x._3) flag = false
				ret
			})
			tmp.map(_._2.token.getWord).toSet
		}
		
	}
	
}
