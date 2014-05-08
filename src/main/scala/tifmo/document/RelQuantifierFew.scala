package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ RuleDo, TermIndex, IEngineCore }

case object RelQuantifierFew extends Relation {
  override def execute[T](ex: Executor, a: T, b: T): Unit = {
    (ex, a, b) match {
      case (ie: IEngineCore, xa: TermIndex, xb: TermIndex) =>
        val a = xa.holder
        val b = xb.holder

        // Downward entailment for both arguments
        ie.foreachSubset(xa, Seq.empty, RuleDo((ie, pred, _) => {
          val aSub = pred.subset.holder
          ie.foreachSubset(b.index, Seq.empty, RuleDo((ie, pred, _) => {
            val xbSub = pred.subset
            ie.claimRL(aSub.index, this, xbSub)
          }))
        }))

        // Few( A, SuperSetOfA ∩ B ) => Few(A, B)
        // Note that it's necessary to use "super set of A" instead of "A",
        // since the inference engine may not be aware of the necessity of exploring theorems involving "A∩B"
        ie.foreachSuperset(xa, Seq.empty, RuleDo((ie, pred, _) => {
          val aSuper = pred.superset.holder
          ie.ifSubsume(b.index, aSuper.index, Seq.empty, RuleDo((ie, pred, _) => {
            val xb = pred.subset
            ie.foreachSuperset(xb, Seq.empty, RuleDo((ie, pred, _) => {
              val xb = pred.subset
              val bSuper = pred.superset.holder
              ie.ifSubsume(ie.getIN(Set(aSuper, bSuper)).index, xb, Seq.empty, RuleDo((ie, pred, _) => {
                ie.claimRL(a.index, this, bSuper.index)
              }))
            }))
          }))
        }))

    }
  }
}
