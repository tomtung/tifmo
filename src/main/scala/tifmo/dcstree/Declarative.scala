package tifmo.dcstree

sealed abstract class Declarative {
  def toStatements: Set[Statement]
}

case class DeclarativePosi(root: DCSTreeNode) extends Declarative {

  val toStatements = {
    root.upward()
    root.downward(null)
    var ret = Set(StatementNotEmpty(root.halfcalc): Statement)
    def recurRel(x: DCSTreeNode) {
      for ((DCSTreeEdgeRelation(role, rel, parentToChild), n) <- x.children) {
        val (l, r) =
          if (parentToChild) {
            (x.germ(role), n.output)
          } else {
            (n.output, x.germ(role))
          }

        ret += StatementRelation(rel, l, r)
      }
      for ((e, n) <- x.children) recurRel(n)
    }
    recurRel(root)
    def recur(x: DCSTreeNode) {
      val rs = x.rseq.dropWhile(_ != x.outRole).toSet
      for ((e, n) <- x.children; if rs.contains(e.inRole)) {
        e match {
          case DCSTreeEdgeNormal(r) => recur(n)
          case DCSTreeEdgeQuantifier(r, qt) =>
            ret += StatementNotEmpty(x.germ(x.rseq.last))
            if (r == x.rseq.last) {
              qt match {
                case QuantifierALL => ret += StatementSubsume(n.output, x.germ(r))
                case QuantifierNO => ret += StatementDisjoint(n.output, x.germ(r))
              }
            }
          case _: DCSTreeEdgeRelation =>
        }
      }
    }
    recur(root)
    ret
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
