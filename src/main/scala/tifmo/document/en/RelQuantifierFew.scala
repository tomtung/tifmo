package tifmo.document.en

import tifmo.dcstree.Relation
import tifmo.document.{ RelLeftNonEmpty, RelConservative, RelLeftDownEntailing, RelRightDownEntailing }

case object RelQuantifierFew
  extends Relation
  with RelLeftDownEntailing
  with RelRightDownEntailing
  with RelConservative
  with RelLeftNonEmpty
