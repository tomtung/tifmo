package tifmo.inference.test

import org.scalatest._
import tifmo.inference.{IEngineCore, Dimension}
import tifmo.dcstree.SemRole

class IEngineCoreSpec extends FlatSpec with Matchers {

  behavior of "An InferenceEngineCore"

  it should "not crash when asked to explore on nothing" in {
    val ie = new IEngineCore()
    noException should be thrownBy ie.explore()
  }

  object SBJ extends SemRole("SBJ")
  object OBJ extends SemRole("OBJ")
  object IOBJ extends SemRole("IOBJ")

  it should """infer "a=c" from "h=a×b=c×d ∧ a≠Ø ∧ b≠Ø"""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ)))
    val b = ie.newTerm(new Dimension(null))
    val c = ie.newTerm(a.dim)
    val d = ie.newTerm(b.dim)
    val h = ie.newTerm(new Dimension(Set(SBJ, OBJ, IOBJ)))

    ie.claimCP(h, Set((a, null:SemRole), (b, IOBJ)))
    ie.claimCP(h, Set((c, null:SemRole), (d, IOBJ)))
    ie.claimNonEmpty(a)
    ie.claimNonEmpty(b)

    // Necessary for the engine to explore relationships between a and c
    ie.getPI(h.holder, Set(SBJ, OBJ))

    ie.explore()

    a.holder.index should equal (c.holder.index)
  }

  it should """not infer "a=c" from "h=a×b=c×d" (without knowing "a≠Ø∧b≠Ø")""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ)))
    val b = ie.newTerm(new Dimension(null))
    val c = ie.newTerm(a.dim)
    val d = ie.newTerm(b.dim)
    val h = ie.newTerm(new Dimension(Set(SBJ, OBJ, IOBJ)))

    ie.claimCP(h, Set((a, null:SemRole), (b, IOBJ)))
    ie.claimCP(h, Set((c, null:SemRole), (d, IOBJ)))

    // Necessary for the engine to explore relationships between a and c
    ie.getPI(h.holder, Set(SBJ, OBJ))

    ie.explore()

    a.holder.index should not equal c.holder.index
  }

  it should """not infer "a=c" from "h=a×b=c×d ∧ a≠Ø" (without knowing "b≠Ø")""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ)))
    val b = ie.newTerm(new Dimension(null))
    val c = ie.newTerm(a.dim)
    val d = ie.newTerm(b.dim)
    val h = ie.newTerm(new Dimension(Set(SBJ, OBJ, IOBJ)))

    ie.claimCP(h, Set((a, null:SemRole), (b, IOBJ)))
    ie.claimCP(h, Set((c, null:SemRole), (d, IOBJ)))
    ie.claimNonEmpty(a)

    // Necessary for the engine to explore relationships between a and c
    ie.getPI(h.holder, Set(SBJ, OBJ))

    ie.explore()

    a.holder.index should not equal c.holder.index
  }
}
