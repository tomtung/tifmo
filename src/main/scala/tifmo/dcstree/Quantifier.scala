package tifmo.dcstree

sealed abstract class Quantifier

case object QuantifierALL extends Quantifier with Serializable

case object QuantifierNO extends Quantifier with Serializable
