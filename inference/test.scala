
import tifmo.knowledge.SemRole
import tifmo.knowledge.SemRole.SemRole
import tifmo.inference.IETerm
import tifmo.inference.IEBasic

object ie extends IEBasic {
	val rANY0 = (args: List[Any]) => args match {
		case (x:IETerm) :: Nil => {
			println("new term! " + x)
		}
		case _ => throw new Exception("weird.")
	}
	val rANY1 = (x: IETerm, args: List[Any]) => conclude(x :: args, rANY0)_
	forAnyTerm(rANY1)
}

ie.newConstant("aSO", Set(SemRole.SBJ, SemRole.OBJ))
ie.newConstant("bOA", Set(SemRole.OBJ, SemRole.ARG))
ie.newConstant("c", null)
val aSO = ie.getConstant("aSO").to
val bOA = ie.getConstant("bOA").to
val c = ie.getConstant("c").to
println("aSO: " + aSO)
println("bOA: " + bOA)
println("c: " + c)
println("")

val aSOcA = ie.getCP(Set((aSO, null), (c, SemRole.ARG)))
val wSbOA = ie.getCP(Set((ie.getW(null), SemRole.SBJ), (bOA, null)))
val aSOcA_wSbOA = ie.getIN(Set(aSOcA, wSbOA))
println("aSOcA: " + aSOcA)
println("wSbOA: " + wSbOA)
println("aSOcA_wSbOA: " + aSOcA_wSbOA)
println(aSOcA_wSbOA.superSets.mkString("\naSOcA_wSbOA.superSets:\n", "\n", "\n"))
ie.claimDisjoint(aSOcA, wSbOA)
println(aSOcA_wSbOA.disjointSets.mkString("\naSOcA_wSbOA.disjointSets:\n", "\n", "\n"))

val aSOcAwT = ie.getCP(Set((aSOcA, null), (ie.getW(null), SemRole.THG)))
val cAwT = ie.getCP(Set((c, SemRole.ARG), (ie.getW(null), SemRole.THG)))
println("aSOcAwT: " + aSOcAwT)
println("cAwT: " + cAwT)
println(aSOcAwT.asCP.mkString("\naSOcAwT.asCP:\n", "\n", "\n"))

val SO_aSOcA_wSbOA = ie.getPI(aSOcA_wSbOA, Set(SemRole.SBJ, SemRole.OBJ))
val S_aSOcA_wSbOA = ie.getPI(aSOcA_wSbOA, Set(SemRole.SBJ))
println("SO_aSOcA_wSbOA: " + SO_aSOcA_wSbOA)
println("S_aSOcA_wSbOA: " + S_aSOcA_wSbOA)
println(S_aSOcA_wSbOA.asPI.mkString("\nS_aSOcA_wSbOA.asPI:\n", "\n", "\n"))

val A_bOA = ie.getPI(bOA, Set(SemRole.ARG))
println("A_bOA: " + A_bOA)
ie.claimSubsume(S_aSOcA_wSbOA, A_bOA)
ie.claimSubsume(A_bOA, c)
println(S_aSOcA_wSbOA.superSets.mkString("\nS_aSOcA_wSbOA.superSets:\n", "\n", "\n"))

val S_aSO = ie.getPI(aSO, Set(SemRole.SBJ))
println("S_aSO: " + S_aSO + "\n")
val rSaSOSub0 = (args: List[Any]) => args match {
	case (x:IETerm) :: Nil => {
		println("S_aSO new subset! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rSaSOSub1 = (x: IETerm, args: List[Any]) => ie.conclude(x :: args, rSaSOSub0)_
ie.foreachDisjoint(S_aSO, Nil, rSaSOSub1)(Set.empty[IETerm])
val rSaSOSuper0 = (args: List[Any]) => args match {
	case (x:IETerm) :: Nil => {
		println("S_aSO new superset! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rSaSOSuper1 = (x: IETerm, args: List[Any]) => ie.conclude(x :: args, rSaSOSuper0)_
ie.foreachDisjoint(S_aSO, Nil, rSaSOSuper1)(Set.empty[IETerm])
val rSaSOMkCP0 = (args: List[Any]) => args match {
	case (x:IETerm) :: (cp:Set[(IETerm, SemRole)]) :: Nil => {
		println("S_aSO new mkcp! " + x + " " + cp)
	}
	case _ => throw new Exception("weird.")
}
val rSaSOMkCP1 = (x: IETerm, cp: Set[(IETerm, SemRole)], args: List[Any]) => ie.conclude(x :: cp :: args, rSaSOMkCP0)_
ie.foreachMkCP(S_aSO, Nil, rSaSOMkCP1)(Set.empty[IETerm])
val rSaSOIsPI0 = (args: List[Any]) => args match {
	case (x:IETerm) :: (r:SemRole) :: Nil => {
		println("S_aSO new ispi! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rSaSOIsPI1 = (x: IETerm, r: SemRole, args: List[Any]) => ie.conclude(x :: r :: args, rSaSOIsPI0)_
ie.foreachIsPI(S_aSO, Nil, rSaSOIsPI1)(Set.empty[IETerm])
val rSaSOIfNE0 = (args: List[Any]) => {
	assert(args.isEmpty)
	println("S_aSO non-empty!!")
}
val rSaSOIfNE1 = (args: List[Any]) => ie.conclude(args, rSaSOIfNE0)_
ie.ifNotEmpty(S_aSO, Nil, rSaSOIfNE1)(Set.empty[IETerm])

val rConCP0 = (args: List[Any]) => args match {
	case (x:IETerm) :: Nil => {
		println("contructed CP! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rConCP1 = (x: IETerm, args: List[Any]) => ie.conclude(x :: args, rConCP0)_
ie.constructCP(Set((aSO, null), (c, SemRole.IOBJ)), Nil, rConCP1)(Set.empty[IETerm])

val rForCP0 = (args: List[Any]) => args match {
	case (x:IETerm) :: Nil => {
		println("found CP! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rForCP1 = (x: IETerm, args: List[Any]) => ie.conclude(x :: args, rForCP0)_
ie.forCPof(Set((aSO, null), (c, SemRole.IOBJ)), Nil, rForCP1)(Set.empty[IETerm])

val rConPIIN0 = (args: List[Any]) => args match {
	case (in:IETerm) :: (pi:IETerm) :: Nil => {
		println("constructed PI! " + pi)
		println("constructed IN! " + in)
		ie.claimDisjoint(in, c)
	}
	case _ => throw new Exception("weird.")
}
val rConPIIN1 = (in: IETerm, args: List[Any]) => ie.conclude(in :: args, rConPIIN0)_
val rConPIIN2 = (pi: IETerm, args: List[Any]) => ie.constructIN(Set(pi, c), pi :: args, rConPIIN1)_
ie.constructPI(bOA, Set(SemRole.OBJ), Nil, rConPIIN2)(Set.empty[IETerm])

val rForPI0 = (args: List[Any]) => args match {
	case (x:IETerm) :: Nil => {
		println("found PI! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rForPI1 = (x: IETerm, args: List[Any]) => ie.conclude(x :: args, rForPI0)_
ie.forPIof(bOA, Set(SemRole.OBJ), Nil, rForPI1)(Set.empty[IETerm])

val rSp0 = (args: List[Any]) => args match {
	case (x:IETerm) :: Nil => {
		println("specific S_aSO! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rSp1 = (x: List[IETerm], args: List[Any]) => ie.conclude(x(0) :: args, rSp0)_
ie.forSpecific(List(S_aSO), Nil, rSp1)(Set.empty[IETerm])

val rForSub0 = (args: List[Any]) => args match {
	case (x:IETerm) :: Nil => {
		println("subset of c! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rForSub1 = (x: IETerm, args: List[Any]) => ie.conclude(x :: args, rForSub0)_
ie.foreachSubset(c, Nil, rForSub1)(Set.empty[IETerm])

val rForSuper0 = (args: List[Any]) => args match {
	case (x:IETerm) :: Nil => {
		println("superset of c! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rForSuper1 = (x: IETerm, args: List[Any]) => ie.conclude(x :: args, rForSuper0)_
ie.foreachSuperset(c, Nil, rForSuper1)(Set.empty[IETerm])

val rIfSub0 = (args: List[Any]) => {
	assert(args.isEmpty)
	println("ifsub & merge!!!")
	ie.claimSubsume(c, S_aSO)
}
val rIfSub1 = (args: List[Any]) => ie.conclude(args, rIfSub0)_
ie.ifSubsume(S_aSO, c, Nil, rIfSub1)(Set.empty[IETerm])

val rForDjt0 = (args: List[Any]) => args match {
	case (x:IETerm) :: Nil => {
		println("c new disjoint! " + x)
	}
	case _ => throw new Exception("weird.")
}
val rForDjt1 = (x: IETerm, args: List[Any]) => ie.conclude(x :: args, rForDjt0)_
ie.foreachDisjoint(c, Nil, rForDjt1)(Set.empty[IETerm])

ie.claimNonEmpty(c)
ie.claimSubsume(S_aSO, c)

ie.explore()

println("SO_aSOcA_wSbOA.asIN: " + SO_aSOcA_wSbOA.ref.to.asIN)
