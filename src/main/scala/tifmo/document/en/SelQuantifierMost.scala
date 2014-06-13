package tifmo.document.en

import tifmo.document.{ IEngineCoreSelectionBase, SelNonEmpty, SelSubsumed }

case object SelQuantifierMost
  extends IEngineCoreSelectionBase
  with SelSubsumed
  with SelNonEmpty
