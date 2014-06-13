package tifmo.document.en

import tifmo.document.{ IEngineCoreSelectionBase, SelCovarianceLike, SelNonEmpty }

case class SelQuantifierAtMost(cardinal: String)
  extends IEngineCoreSelectionBase
  with SelCovarianceLike
  with SelNonEmpty
