package tifmo.document

case object SelQuantifierLotOf
  extends IEngineCoreSelectionBase
  // The following line is commented out to make this quantifier non-monotone in the first argument
  // This matches the interpretation of FraCaS people
  //  with SelContravarianceLike
  with SelWithNonEmptyIntersection
