package tifmo.document.en

import tifmo.document.{ IEngineCoreSelectionBase, SelContravarianceLike, SelNonEmpty, SelSubsumed }

case object SelQuantifierAFew
  extends IEngineCoreSelectionBase
  with SelContravarianceLike
  with SelSubsumed
  with SelNonEmpty
