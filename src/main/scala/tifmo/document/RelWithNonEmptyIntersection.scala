package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ TermIndex, IEngineCore }

trait RelWithNonEmptyIntersection extends Relation {
  override def execute[T](ex: Executor, a: T, b: T) {
    super.execute(ex, a, b)
    (ex, a, b) match {
      case (ie: IEngineCore, xa: TermIndex, xb: TermIndex) =>
        ie.claimNonEmpty(ie.getIN(Set(xa.holder, xb.holder)).index)

      case _ => // Nothing
    }
  }
}
