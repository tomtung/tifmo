package tifmo.document

import tifmo.dcstree.Relation

case object RelQuantifierLotOf
  extends Relation
  with RelUpUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative
