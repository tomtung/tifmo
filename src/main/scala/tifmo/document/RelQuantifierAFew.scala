package tifmo.document

import tifmo.dcstree.Relation

case object RelQuantifierAFew
  extends Relation
  with RelLeftUpEntailing
  with RelRightUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative
  with RelEntailedBySubsumption
