
import tifmo.knowledge.SemRole
import tifmo.knowledge.SemRole.SemRole
import tifmo.knowledge.WordInfo
import tifmo.inference.IETerm
import tifmo.inference.IEBasic

class SimpleWord(val lex: String) extends WordInfo {
	val sign = true
	val isStopwd = false
}

val ie = new IEBasic

ie.newConstant("aSO", Set(SemRole.SBJ, SemRole.OBJ))
ie.newConstant("bOA", Set(SemRole.OBJ, SemRole.ARG))
ie.newConstant("c", null)
ie.newConstant("d", null)
ie.newConstant("e", null)

val aSO = ie.getConstant(new SimpleWord("aSO"))
val bOA = ie.getConstant(new SimpleWord("bOA"))
val c = ie.getConstant(new SimpleWord("c"))
val d = ie.getConstant(new SimpleWord("d"))
val e = ie.getConstant(new SimpleWord("e"))
println("aSO: " + aSO)
println("bOA: " + bOA)
println("c: " + c)
println("d: " + d)
println("e: " + e)
println("")

ie.claimSubsume(c, d)
ie.claimSubsume(d, e)
println(c.superSets)

val aSOdA = ie.getCP(Set((aSO, null), (d, SemRole.ARG)))
val wSOeA = ie.getCP(Set((ie.getW(aSO.roles), null), (e, SemRole.ARG)))
println("aSOdA: " + aSOdA)
println("wSOeA: " + wSOeA)

println("aSOdA.superSets: " + aSOdA.superSets)
ie.explore()
println("aSOdA.superSets: " + aSOdA.superSets)

val wSbOA = ie.getCP(Set((ie.getW(null), SemRole.SBJ), (bOA, null)))
val aSOdA_wSbOA = ie.getIN(Set(aSOdA, wSbOA))
val wSOeA_wSbOA = ie.getIN(Set(wSOeA, wSbOA))

println("wSbOA: " + wSbOA)
println("aSOdA_wSbOA: " + aSOdA_wSbOA)
println("wSOeA_wSbOA: " + wSOeA_wSbOA)

println("aSOdA_wSbOA.superSets: " + aSOdA_wSbOA.superSets)
ie.explore()
println("aSOdA_wSbOA.superSets: " + aSOdA_wSbOA.superSets)

val O_aSOdA_wSbOA = ie.getPI(aSOdA_wSbOA, Set(SemRole.OBJ))
val O_wSOeA_wSbOA = ie.getPI(wSOeA_wSbOA, Set(SemRole.OBJ))

println("O_aSOdA_wSbOA: " + O_aSOdA_wSbOA)
println("O_wSOeA_wSbOA: " + O_wSOeA_wSbOA)

println("O_aSOdA_wSbOA.superSets: " + O_aSOdA_wSbOA.superSets)
ie.explore()
println("O_aSOdA_wSbOA.superSets: " + O_aSOdA_wSbOA.ref.to.superSets)

val S_aSOdA_wSbOA = ie.getPI(aSOdA_wSbOA, Set(SemRole.SBJ))
val S_wSOeA_wSbOA = ie.getPI(wSOeA_wSbOA, Set(SemRole.SBJ))

println("S_aSOdA_wSbOA: " + S_aSOdA_wSbOA)
println("S_wSOeA_wSbOA: " + S_wSOeA_wSbOA)

println("S_aSOdA_wSbOA.superSets: " + S_aSOdA_wSbOA.superSets)
ie.explore()
println("S_aSOdA_wSbOA.superSets: " + S_aSOdA_wSbOA.ref.to.superSets)





/*
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
//println(aSOcAwT.iscps.mkString("\naSOcAwT.asCP:\n", "\n", "\n"))

val SO_aSOcA_wSbOA = ie.getPI(aSOcA_wSbOA, Set(SemRole.SBJ, SemRole.OBJ))
val S_aSOcA_wSbOA = ie.getPI(aSOcA_wSbOA, Set(SemRole.SBJ))
println("SO_aSOcA_wSbOA: " + SO_aSOcA_wSbOA)
println("S_aSOcA_wSbOA: " + S_aSOcA_wSbOA)
//println(S_aSOcA_wSbOA.ispis.mkString("\nS_aSOcA_wSbOA.asPI:\n", "\n", "\n"))

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
*/
