package tifmo.document

import tifmo.dcstree.Relation

case object RelQuantifierAFew
  extends Relation
  with RelUpUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative

