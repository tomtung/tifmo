package tifmo.document

import tifmo.dcstree.{ Declarative, Relation, Executor, Selection }
import tifmo.inference._
import tifmo.onthefly.AEngine

case class SelAtLeast(cardinal: String) extends Selection {

  // TODO extract a trait/baseclass "ContravarianceSelection"?

  import SelAtLeast._

  override def execute[T](ex: Executor, x: T): T = {
    (ex, x) match {
      case (ie: IEngineCore, tm: Term) =>
        ie.getFunc(SelAtLeastFunc, Seq(null, tm), cardinal).asInstanceOf[T]
      case (ae: AEngine, toProve: Function1[_, _]) =>
        ((d: Declarative) => {}).asInstanceOf[T]
    }
  }
}

object SelAtLeast {

  private[document] object SelAtLeastFunc extends IEFunction {
    override def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any): Unit = {
      tms match {
        case Seq(sel, base) =>
          val cardinal = param.asInstanceOf[String]
          val selTerm = sel.holder
          ie.claimNonEmpty(sel)
          ie.claimNonEmpty(ie.getIN(Set(sel.holder, base.holder)).index)
          ie.claimRL(sel, RelAtLeast(cardinal), base)
          ie.foreachSuperset(base, Seq.empty,
            RuleDo((ie, p, _) => {
              val supersetBase = p.superset
              ie.foreachXRLB(supersetBase, Seq.empty,
                RuleDo((ie, p, _) => {
                  p.rl match {
                    case RelAtLeast(`cardinal`) =>
                      val supersetSel = p.a
                      ie.claimSubsume(supersetSel, selTerm.index)
                  }
                })
              )
            })
          )
      }
    }

    override def headDim(tms: Seq[Term], param: Any): Dimension = tms(1).dim
  }

  private[document] case class RelAtLeast(cardinal: String) extends Relation {
    override def execute[T](ex: Executor, sel: T, base: T): Unit = {
      // This relation is just a marker, do nothing
    }
  }

}
