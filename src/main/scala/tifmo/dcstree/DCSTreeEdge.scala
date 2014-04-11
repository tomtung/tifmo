package tifmo.dcstree

sealed abstract class DCSTreeEdge {

  def inRole: SemRole

}

case class DCSTreeEdgeNormal(inRole: SemRole) extends DCSTreeEdge

case class DCSTreeEdgeQuantifier(inRole: SemRole, quantifier: Quantifier) extends DCSTreeEdge

/**
 * A DCS tree edge marked with relation
 * @param inRole The role of the parent node on this edge
 * @param relation The relation this edge is marked with
 * @param parentToChild `true` if parent is the first parameter of `relation` while `children` being the second, and vice versa.
 */
case class DCSTreeEdgeRelation(inRole: SemRole, relation: Relation, parentToChild: Boolean) extends DCSTreeEdge
