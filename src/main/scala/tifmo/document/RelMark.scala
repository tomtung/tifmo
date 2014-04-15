package tifmo.document

import tifmo.dcstree.{ Executor, Relation }

case class RelMark(marker: Any) extends Relation {
  override def execute[T](ex: Executor, sel: T, base: T): Unit = {
    // This relation is just a marker, do nothing
  }
}
