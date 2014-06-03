package tifmo.document

import tifmo.dcstree.Relation

case object RelQuantifierFew
  extends Relation
  with RelLeftDownEntailing
  with RelRightDownEntailing
  with RelConservative
