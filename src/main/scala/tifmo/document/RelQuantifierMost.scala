package tifmo.document

import tifmo.dcstree.Relation

object RelQuantifierMost
  extends Relation
  with RelRightUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative
  with RelEntailedBySubsumption
