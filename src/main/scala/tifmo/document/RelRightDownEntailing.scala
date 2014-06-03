package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ RuleDo, TermIndex, IEngineCore }

trait RelRightDownEntailing extends Relation {
  override def execute[T](ex: Executor, a: T, b: T) {
    super.execute(ex, a, b)
    (ex, a, b) match {
      case (ie: IEngineCore, xa: TermIndex, xb: TermIndex) =>
        val a = xa.holder
        ie.foreachSubset(xb, Seq.empty, RuleDo((ie, pred, _) => {
          val xbSub = pred.subset
          ie.claimRL(a.index, this, xbSub)
        }))
      case _ => // Nothing
    }
  }
}
