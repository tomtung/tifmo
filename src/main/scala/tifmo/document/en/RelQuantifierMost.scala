package tifmo.document.en

import tifmo.dcstree.Relation
import tifmo.document.{ RelConservative, RelEntailedBySubsumption, RelRightUpEntailing, RelWithNonEmptyIntersection }

object RelQuantifierMost
  extends Relation
  with RelRightUpEntailing
  with RelWithNonEmptyIntersection
  with RelConservative
  with RelEntailedBySubsumption
