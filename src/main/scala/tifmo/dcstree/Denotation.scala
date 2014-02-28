package tifmo

import scala.runtime.ScalaRunTime

package dcstree {
	
	sealed abstract class Denotation {
		val roles: Set[SemRole]
		@transient override lazy val hashCode = ScalaRunTime._hashCode(this.asInstanceOf[Product])
	}
	
	case class DenotationW(roles: Set[SemRole]) extends Denotation
	
	case class DenotationWordSign(roles: Set[SemRole], word: WordBase, sign: Boolean) extends Denotation
	
	case class DenotationIN(x: Set[Denotation]) extends Denotation {
		val roles = x.head.roles
		assert(x.forall(_.roles == roles))
	}
	
	case class DenotationCP(x: Set[(Denotation, SemRole)]) extends Denotation {
		assert(x.forall(xx => (xx._1.roles.size == 1 && xx._2 != null) ^ (xx._1.roles.size >= 2 && xx._2 == null)))
		val roles = x.flatMap(xx => if (xx._2 == null) xx._1.roles else Set(xx._2))
		assert(roles.size == (0 /: x)(_ + _._1.roles.size))
	}
	
	case class DenotationPI(x: Denotation, roles: Set[SemRole]) extends Denotation {
		assert(roles.subsetOf(x.roles))
		assert(roles != x.roles)
	}
	
	case class DenotationDI(qt: Quantifier, x: Denotation, y: Denotation, r: SemRole) extends Denotation {
		assert(x.roles.contains(r))
		assert(y.roles.size == 1)
		val roles = x.roles - r
	}
	
	case class DenotationSelection(sel: Selection, x: Denotation) extends Denotation {
		val roles = x.roles
	}
	
}
