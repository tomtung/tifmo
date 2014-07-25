package tifmo.document.en

import tifmo.dcstree.Relation
import tifmo.document.{ RelLeftNonEmpty, RelConservative, RelLeftDownEntailing, RelRightDownEntailing }

case class RelQuantifierAtMost(cardinal: String)
  extends Relation
  with RelLeftDownEntailing
  with RelRightDownEntailing
  with RelConservative
  with RelLeftNonEmpty

