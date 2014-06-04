package tifmo.document

case class SelQuantifierAtMost(cardinal: String)
  extends IEngineCoreSelectionBase
  with SelCovarianceLike
  with SelNonEmpty
