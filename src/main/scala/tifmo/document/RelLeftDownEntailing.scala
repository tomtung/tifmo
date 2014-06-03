package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ RuleDo, TermIndex, IEngineCore }

trait RelLeftDownEntailing extends Relation {
  override def execute[T](ex: Executor, a: T, b: T) {
    super.execute(ex, a, b)
    (ex, a, b) match {
      case (ie: IEngineCore, xa: TermIndex, xb: TermIndex) =>
        val b = xb.holder
        ie.foreachSubset(xa, Seq.empty, RuleDo((ie, pred, _) => {
          val xaSub = pred.subset
          ie.claimRL(xaSub, this, b.index)
        }))
      case _ => // Nothing
    }
  }
}

