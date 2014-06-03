package tifmo.document

import tifmo.dcstree.Relation

case object RelQuantifierLotOf
  extends Relation
  // The following line is commented out to make this quantifier non-monotone in the first argument
  // This matches the interpretation of FraCaS people
  //  with RelLeftUpEntailing
  with RelRightUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative
