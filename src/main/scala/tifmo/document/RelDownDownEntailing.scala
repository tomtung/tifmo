package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ RuleDo, TermIndex, IEngineCore }

trait RelDownDownEntailing extends Relation {
  override def execute[T](ex: Executor, a: T, b: T) {
    super.execute(ex, a, b)
    (ex, a, b) match {
      case (ie: IEngineCore, xa: TermIndex, xb: TermIndex) =>
        val b = xb.holder
        ie.foreachSubset(xa, Seq.empty, RuleDo((ie, pred, _) => {
          val aSub = pred.subset.holder
          ie.foreachSubset(b.index, Seq.empty, RuleDo((ie, pred, _) => {
            val xbSub = pred.subset
            ie.claimRL(aSub.index, this, xbSub)
          }))
        }))

      case _ => // Nothing
    }
  }
}
