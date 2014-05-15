package tifmo.document

import tifmo.dcstree.Relation

case class RelQuantifierAtLeast(cardinal: String)
  extends Relation
  with RelUpUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative
