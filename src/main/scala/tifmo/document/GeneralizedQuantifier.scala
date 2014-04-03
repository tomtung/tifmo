package tifmo.document

sealed abstract class GeneralizedQuantifier

case object MostQuantifier extends GeneralizedQuantifier

case class AtMostQuantifier(cardinal: String) extends GeneralizedQuantifier

case class AtLeastQuantifier(cardinal: String) extends GeneralizedQuantifier
