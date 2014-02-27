import tifmo.inference.IEngineCore

val ie = new IEngineCore()

import tifmo.dcstree.SemRole

object SBJ extends SemRole("SBJ")

object OBJ extends SemRole("OBJ")

object IOBJ extends SemRole("IOBJ")

import tifmo.inference.Dimension

val a = ie.newTerm(new Dimension(Set(SBJ, OBJ)))

val b = ie.newTerm(new Dimension(null))

val c = ie.newTerm(a.dim)

val d = ie.newTerm(b.dim)

val h = ie.newTerm(new Dimension(Set(SBJ, OBJ, IOBJ)))

ie.claimCP(h, Set((a, null:SemRole), (b, IOBJ)))

ie.claimCP(h, Set((c, null:SemRole), (d, IOBJ)))

ie.claimNonEmpty(a)

ie.claimNonEmpty(b)

ie.explore()

