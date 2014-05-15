package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ RuleDo, TermIndex, IEngineCore }

trait RelUpUpEntailing extends Relation {
  override def execute[T](ex: Executor, a: T, b: T) {
    super.execute(ex, a, b)
    (ex, a, b) match {
      case (ie: IEngineCore, xa: TermIndex, xb: TermIndex) =>
        val b = xb.holder
        ie.foreachSuperset(xa, Seq.empty, RuleDo((ie, pred, _) => {
          val aSuper = pred.superset.holder
          ie.foreachSuperset(b.index, Seq.empty, RuleDo((ie, pred, _) => {
            val xbSuper = pred.superset
            ie.claimRL(aSuper.index, this, xbSuper)
          }))
        }))

      case _ => // Nothing
    }
  }
}
