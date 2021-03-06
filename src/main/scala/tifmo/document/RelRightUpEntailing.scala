package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ RuleDo, TermIndex, IEngineCore }

trait RelRightUpEntailing extends Relation {
  override def execute[T](ex: Executor, a: T, b: T) {
    super.execute(ex, a, b)
    (ex, a, b) match {
      case (ie: IEngineCore, xa: TermIndex, xb: TermIndex) =>
        val a = xa.holder
        ie.foreachSuperset(xb, Seq.empty, RuleDo((ie, pred, _) => {
          val xbSuper = pred.superset
          ie.claimRL(a.index, this, xbSuper)
        }))

      case _ => // Nothing
    }
  }
}
