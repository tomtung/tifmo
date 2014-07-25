package tifmo.document

import tifmo.dcstree.{ Executor, Relation }
import tifmo.inference.{ TermIndex, IEngineCore }

trait RelLeftNonEmpty extends Relation {
  override def execute[T](ex: Executor, a: T, b: T) {
    super.execute(ex, a, b)
    (ex, a) match {
      case (ie: IEngineCore, xa: TermIndex) =>
        ie.claimNonEmpty(xa)

      case _ => // Nothing
    }
  }
}
