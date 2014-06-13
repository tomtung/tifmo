package tifmo.document.en

import tifmo.dcstree.Relation
import tifmo.document.{ RelConservative, RelLeftDownEntailing, RelRightDownEntailing }

case object RelQuantifierFew
  extends Relation
  with RelLeftDownEntailing
  with RelRightDownEntailing
  with RelConservative
