package tifmo.document.en

import tifmo.dcstree.Relation
import tifmo.document.{ RelConservative, RelLeftUpEntailing, RelRightUpEntailing, RelWithNonEmptyIntersection }

case class RelQuantifierAtLeast(cardinal: String)
  extends Relation
  with RelLeftUpEntailing
  with RelRightUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative
