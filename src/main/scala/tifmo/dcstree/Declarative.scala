package tifmo.dcstree

sealed abstract class Declarative {
  def toStatements: Set[Statement]
}

case class DeclarativePosi(root: DCSTreeNode) extends Declarative {

  val toStatements: Set[Statement] = {
    root.upward()
    root.downward(null)

    val builder = Set.newBuilder[Statement]

    def recurRel(parentNode: DCSTreeNode) {
      for ((DCSTreeEdgeRelation(role, rel, parentToChild), childNode) <- parentNode.children) {
        val (l, r) =
          if (parentToChild) {
            (parentNode.germ(role), childNode.output)
          } else {
            (childNode.output, parentNode.germ(role))
          }

        builder += StatementRelation(rel, l, r)
      }

      for ((_, childNode) <- parentNode.children) {
        recurRel(childNode)
      }
    }
    recurRel(root)

    var hasQuantifierNo = false
    def recur(parentNode: DCSTreeNode) {
      val rs = parentNode.rseq.dropWhile(_ != parentNode.outRole).toSet
      for ((edge, childNode) <- parentNode.children; if rs.contains(edge.inRole)) {
        edge match {
          case DCSTreeEdgeNormal(_) => recur(childNode)
          case DCSTreeEdgeQuantifier(r, qt) =>
            if (r == parentNode.rseq.last) {
              qt match {
                case QuantifierALL =>
                  builder += StatementNotEmpty(parentNode.germ(parentNode.rseq.last))
                  builder += StatementNotEmpty(childNode.output)
                  builder += StatementSubsume(childNode.output, parentNode.germ(r))
                case QuantifierNO =>
                  builder += StatementDisjoint(childNode.output, parentNode.germ(r))
                  hasQuantifierNo = true
              }
            } else {
              builder += StatementNotEmpty(parentNode.germ(parentNode.rseq.last))
            }
          case _: DCSTreeEdgeRelation =>
        }
      }
    }

    recur(root)
    if (!hasQuantifierNo) {
      builder += StatementNotEmpty(root.halfcalc)
    }

    builder.result()
  }

}

case class DeclarativeNega(root: DCSTreeNode) extends Declarative {

  val toStatements = {
    root.upward()
    root.downwardNega(null)
    Set(StatementDisjoint(root.halfcalc, root.halfcalc): Statement)
  }

}

case class DeclarativeNotEmptyRef(ref: Ref) extends Declarative {

  def toStatements = Set(StatementNotEmpty(ref.getDenotation): Statement)

}

case class DeclarativeSubRef(sub: Ref, sup: Ref) extends Declarative {

  def toStatements = Set(StatementSubsume(sub.getDenotation, sup.getDenotation): Statement)

}

case class DeclarativeDjtRef(a: Ref, b: Ref) extends Declarative {

  def toStatements = Set(StatementSubsume(a.getDenotation, b.getDenotation): Statement)

}

case class DeclarativeRel(rel: Relation, a: Ref, b: Ref) extends Declarative {

  def toStatements = Set(StatementRelation(rel, a.getDenotation, b.getDenotation): Statement)

}

case class DeclarativeSubsume(sub: DCSTreeNode, sup: DCSTreeNode) extends Declarative {

  def toStatements = {
    val crs = sub.approx.roles intersect sup.approx.roles
    if (crs.isEmpty) {
      Set.empty[Statement]
    } else {
      val tmpsub = if (crs == sub.approx.roles) sub.approx else DenotationPI(sub.approx, crs)
      val tmpsup = if (crs == sup.approx.roles) sup.approx else DenotationPI(sup.approx, crs)
      Set(StatementSubsume(tmpsub, tmpsup): Statement)
    }
  }

}
