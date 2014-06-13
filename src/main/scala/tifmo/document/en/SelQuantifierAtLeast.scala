package tifmo.document.en

import tifmo.document.{ IEngineCoreSelectionBase, SelContravarianceLike, SelWithNonEmptyIntersection }

case class SelQuantifierAtLeast(cardinal: String)
  extends IEngineCoreSelectionBase
  with SelContravarianceLike
  with SelWithNonEmptyIntersection
