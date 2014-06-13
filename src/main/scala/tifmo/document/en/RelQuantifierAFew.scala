package tifmo.document.en

import tifmo.dcstree.Relation
import tifmo.document._

case object RelQuantifierAFew
  extends Relation
  with RelLeftUpEntailing
  with RelRightUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative
  with RelEntailedBySubsumption
