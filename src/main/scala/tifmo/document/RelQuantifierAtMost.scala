package tifmo.document

import tifmo.dcstree.Relation

case class RelQuantifierAtMost(cardinal: String)
  extends Relation
  with RelLeftDownEntailing
  with RelRightDownEntailing
  with RelConservative

