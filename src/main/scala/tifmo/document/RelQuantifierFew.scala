package tifmo.document

import tifmo.dcstree.Relation

case object RelQuantifierFew
  extends Relation
  with RelDownDownEntailing
  with RelConservative { override def isRightDownwardEntailing = true }

