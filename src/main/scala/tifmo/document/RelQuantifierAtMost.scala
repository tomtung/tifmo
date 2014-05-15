package tifmo.document

import tifmo.dcstree.Relation

case class RelQuantifierAtMost(cardinal: String)
  extends Relation
  with RelDownDownEntailing
  with RelConservative
