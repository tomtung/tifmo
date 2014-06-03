package tifmo.document

import tifmo.dcstree.Relation

case class RelQuantifierAtLeast(cardinal: String)
  extends Relation
  with RelLeftUpEntailing
  with RelRightUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative
