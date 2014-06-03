package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ RuleDo, TermIndex, IEngineCore }

trait RelLeftUpEntailing extends Relation {
  override def execute[T](ex: Executor, a: T, b: T) {
    super.execute(ex, a, b)
    (ex, a, b) match {
      case (ie: IEngineCore, xa: TermIndex, xb: TermIndex) =>
        val b = xb.holder
        ie.foreachSuperset(xa, Seq.empty, RuleDo((ie, pred, _) => {
          val xaSuper = pred.superset
          ie.claimRL(xaSuper, this, b.index)
        }))

      case _ => // Nothing
    }
  }
}
