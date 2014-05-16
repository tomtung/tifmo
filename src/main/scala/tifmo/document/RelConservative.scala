package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ RuleDo, TermIndex, IEngineCore }

trait RelConservative extends Relation {

  def isRightDownwardEntailing: Boolean = false

  override def execute[T](ex: Executor, a: T, b: T) {
    super.execute(ex, a, b)
    (ex, a, b) match {
      case (ie: IEngineCore, xa: TermIndex, xb: TermIndex) =>
        val a = xa.holder
        val b = xb.holder

        if (!isRightDownwardEntailing) {
          // R(A, B) => R(A, A∩B)
          ie.claimRL(xa, this, ie.getIN(Set(a, b)).index)
          // R(A, A∩B) => R(A, B)
          ie.ifSubsume(b.index, xa, Seq.empty, RuleDo((ie, pred, _) => {
            val xb = pred.subset
            ie.foreachSuperset(xb, Seq.empty, RuleDo((ie, pred, _) => {
              val xb = pred.subset
              val bSuper = pred.superset.holder
              ie.ifSubsume(ie.getIN(Set(a, bSuper)).index, xb, Seq.empty, RuleDo((ie, pred, _) => {
                ie.claimRL(a.index, this, bSuper.index)
              }))
            }))
          }))
        } else {
          // R(A, SuperSetOfA ∩ B ) => R(A, B)
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

      case _ => // Nothing
    }
  }
}
