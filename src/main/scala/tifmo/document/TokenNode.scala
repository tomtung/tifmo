package tifmo.document

import tifmo.dcstree.{ SemRole, Quantifier, Relation, Selection }

import scala.collection.{ mutable, immutable }

class TokenNode(val token: Token) {

  /**
   * Set to `false` if the word is negated.
   */
  var sign = true

  /**
   * The calculation order of the semantic roles of the token.
   */
  var rseq: immutable.Seq[SemRole] = null

  /**
   * The output semantic role.
   */
  var outRole = null: SemRole

  /**
   * The universal quantifier. If none set it to `null`.
   */
  var quantifier = null: Quantifier

  /**
   * The relation with its parent. If none set it to `null`.
   * The direction of this relation is determined by `relationFromParent`.
   */
  var relation = null: Relation

  /**
   * Indicate the direction of relation.
   * If set to true, the parent of the current node would be the first argument of the relation,
   * while the node itself being second, and vice versa.
   */
  var relationFromParent: Boolean = false

  /**
   * The selection operator. If none set it to `null`.
   */
  var selection = null: Selection

  /**
   * Whether the node is in conjunction with its parent.
   */
  var conj = false

  /**
   * If this is the root node, setting `rootNeg` to `true` can negate the whole sentence.
   */
  var rootNeg = false

  ///////////////////////////////////////////////////////
  private var prt = null: TokenNode
  private val cs = mutable.Set.empty[(SemRole, TokenNode)]

  /**
   * Get parent node.
   */
  def parent = prt

  /**
   * Get all children (including conjunction).
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

  /**
   * Add a conjunction.
   */
  def addConjunction(n: TokenNode) {
    if (n == this) {
      // ignore
    } else if (n.parent == this) {
      cs.retain(_._2 != n)
      cs += ((null, n))
      n.conj = true
    } else {
      assert(n.parent == null)
      cs += ((null, n))
      n.prt = this
      n.conj = true
    }
  }

}
