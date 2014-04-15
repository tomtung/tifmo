package tifmo.document

import tifmo.dcstree.Relation

// This relation is just a marker, do nothing
case class RelMark(marker: Any) extends Relation
