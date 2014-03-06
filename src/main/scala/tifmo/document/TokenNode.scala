package tifmo

import dcstree.SemRole
import dcstree.Quantifier
import dcstree.Relation
import dcstree.Selection
import extension.SelNum
import extension.SelSup

import scala.collection.mutable

package document {
	
	class TokenNode(val token: Token) extends Ordered[TokenNode] {
		
		var sign = true
		
		/**
		 * The calculation order of the semantic roles of the token.
		 */
		var rseq = null:Seq[SemRole]
		
		/**
		 * The output semantic role.
		 */
		var outRole = null:SemRole
		
		/**
		 * The universal quantifier. If none set it to `null`.
		 */
		var quantifier = null:Quantifier
		
		/**
		 * A relation. If none set it to `null`.
		 */
		var relation = null:Relation
		
		/**
		 * The selection operator. If none set it to `null`.
		 */
		var selection = null:Selection
		
		/**
		 * Whether the node is in conjunction with its parent.
		 */
		var conj = false
		
		var rootNeg = false
		
		///////////////////////////////////////////////////////
		private var prt = null:TokenNode
		private val cs = mutable.Set.empty[(SemRole, TokenNode)]
		
		/**
		 * Get parent node.
		 */
		def parent = prt
		
		/**
		 * Get children.
		 */
		def children = cs.toSet
		
		def locRoles: Set[SemRole] = {
			var tmp = for ((r, n) <- children; if !n.conj) yield r
			if (outRole != null) tmp += outRole
			if (selection != null) selection match {
				case SelSup(nm, r) => tmp += r
				case SelNum(nm, r) => tmp += r
				case _ => {}
			}
			if (conj) tmp ++= parent.locRoles
			tmp
		}
		
		/**
		 * Cut this node from its parent.
		 */
		def cut() {
			if (prt != null) {
				prt.cs.retain(_._2 != this)
			}
			prt = null
		}
		
		/**
		 * Add a child.
		 */
		def addChild(r: SemRole, n: TokenNode) {
			if (n == this) {
				// ignore
			} else if (n.parent == this) {
				cs.retain(_._2 != n)
				cs += ((r, n))
			} else {
				assert(n.parent == null)
				cs += ((r, n))
				n.prt = this
			}
		}
		
		///////////////////////////////////////////////////////
		def compare(that: TokenNode) = token.id - that.token.id
		
	}
	
}
