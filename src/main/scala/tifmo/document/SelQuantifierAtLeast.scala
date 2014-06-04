package tifmo.document

case class SelQuantifierAtLeast(cardinal: String)
  extends IEngineCoreSelectionBase
  with SelContravarianceLike
  with SelWithNonEmptyIntersection
