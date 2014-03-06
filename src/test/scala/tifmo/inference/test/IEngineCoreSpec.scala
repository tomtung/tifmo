package tifmo.inference.test

import org.scalatest._
import tifmo.inference._
import tifmo.dcstree.{Executor, SemRole, Relation}

class IEngineCoreSpec extends FlatSpec with Matchers {

  behavior of "An InferenceEngineCore"

  it should "not crash when asked to explore on nothing" in {
    val ie = new IEngineCore()
    noException should be thrownBy ie.explore()
    ie.hasContradiction shouldBe false
  }

  object SBJ extends SemRole("SBJ")

  object OBJ extends SemRole("OBJ")

  object IOBJ extends SemRole("IOBJ")

  it should "infer that the universal set W is not empty (axiom 1)" in {
    val ie = new IEngineCore()
    val w = ie.getW(new Dimension(null)).holder

    ie.hasContradiction shouldBe false
    w.isW shouldBe true
    w.knownNE shouldBe true
  }

  it should """infer "A⊂Wᵣ₁×Wᵣ₂" from "type(A)=(r₁,r₂)" (axiom 2)""" in {
    val ie = new IEngineCore()
    val term = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val w = ie.getW(new Dimension(Set(SBJ, OBJ))).holder

    // No need to call explore()
    term.hasSuper(w) shouldBe true
  }

  it should "infer that any set is a subset of itself (axiom 3)" in {
    val ie = new IEngineCore()
    val term1 = ie.new1DTerm().holder
    val term2 = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder

    // No need to call explore()
    term1.hasSuper(term1) shouldBe true
    term2.hasSuper(term2) shouldBe true
    term1.hasSub(term1) shouldBe true
    term2.hasSub(term2) shouldBe true
  }

  it should """infer "A=B" from "A⊂B∧B⊂A" (axiom 4)""" in {
    val ie = new IEngineCore()

    val a, b = ie.new1DTerm().holder

    a.index should not equal b.index

    ie.claimSubsume(a.index, b.index)
    ie.claimSubsume(b.index, a.index)
    ie.explore()

    ie.hasContradiction shouldBe false
    a.index shouldEqual b.index
  }

  it should """infer that subset relation is transitive (axiom 5)""" in {
    val ie = new IEngineCore()

    val a, b, c = ie.new1DTerm().holder

    a.hasSuper(c) shouldBe false
    c.hasSub(a) shouldBe false

    ie.claimSubsume(a.index, b.index)
    ie.claimSubsume(b.index, c.index)
    ie.explore()

    ie.hasContradiction shouldBe false
    a.hasSuper(c) shouldBe true
    c.hasSub(a) shouldBe true
  }

  it should """infer that "A∩B⊂A" (axiom 6)""" in {
    val ie = new IEngineCore()

    val a, b = ie.new1DTerm().holder
    val intersect = ie.getIN(Set(a, b))

    ie.explore()

    ie.hasContradiction shouldBe false
    intersect.hasSuper(a) shouldBe true
    a.hasSub(intersect) shouldBe true
  }

  it should """infer "C⊂A∩B" from "C⊂A∧C⊂B" (axiom 7)""" in {
    val ie = new IEngineCore()

    val a, b, c = ie.new1DTerm().holder
    ie.claimSubsume(c.index, a.index)
    ie.claimSubsume(c.index, b.index)

    val intersect = ie.getIN(Set(a, b))

    ie.explore()

    ie.hasContradiction shouldBe false
    c.hasSuper(intersect) shouldBe true
    intersect.hasSub(c) shouldBe true
  }

  it should """infer "B≠∅" from "A≠∅∧A⊂B" (axiom 8)""" in {
    val ie = new IEngineCore()

    val a, b = ie.new1DTerm().holder
    b.knownNE shouldBe false

    ie.claimNonEmpty(a.index)
    ie.claimSubsume(a.index, b.index)

    ie.explore()

    b.knownNE shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer that disjoint relation is commutative (axiom 9)""" in {
    val ie = new IEngineCore()

    val a, b = ie.new1DTerm().holder

    a.disjointTo(b) shouldBe false
    b.disjointTo(a) shouldBe false

    ie.claimDisjoint(a.index, b.index)
    ie.explore()

    a.disjointTo(b) shouldBe true
    b.disjointTo(a) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "C∥B" from "A∥B∧C⊂A" (axiom 10)""" in {
    val ie = new IEngineCore()

    val a, b, c = ie.new1DTerm().holder

    c.disjointTo(b) shouldBe false

    ie.claimDisjoint(a.index, b.index)
    ie.claimSubsume(c.index, a.index)
    ie.explore()

    c.disjointTo(b) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "A∥B" from "πᵣ(A)∥πᵣ(B)", assuming "type(A)=type(B)" (axiom 11)""" in {
    val ie = new IEngineCore()

    val a, b = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder

    a.disjointTo(b) shouldBe false

    val projA = ie.getPI(a, Set(SBJ))
    val projB = ie.getPI(b, Set(SBJ))

    ie.claimDisjoint(projA.index, projB.index)
    ie.explore()

    a.disjointTo(b) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "πᵣ(A)∥πᵣ(A)" from "A∥A" (axiom 12)""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val projA = ie.getPI(a, Set(SBJ))

    projA.selfDisjoint shouldBe false

    ie.claimDisjoint(a.index, a.index)
    ie.explore()

    projA.selfDisjoint shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "πᵣ(A)≠∅" from "A≠∅", and vice versa (axiom 13)""" in {
    val ie = new IEngineCore()

    {
      val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
      val projA = ie.getPI(a, Set(SBJ))

      projA.knownNE shouldBe false

      ie.claimNonEmpty(a.index)
      ie.explore()

      projA.knownNE shouldBe true
    }

    {
      val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
      val projA = ie.getPI(a, Set(SBJ))

      a.knownNE shouldBe false

      ie.claimNonEmpty(projA.index)
      ie.explore()

      a.knownNE shouldBe true
    }

    ie.hasContradiction shouldBe false
  }

  it should """infer "πᵣ(A)⊂πᵣ(B)" from "A⊂B" (axiom 14)""" in {
    val ie = new IEngineCore()

    val a, b = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val projA = ie.getPI(a, Set(SBJ))
    val projB = ie.getPI(b, Set(SBJ))

    projA.hasSuper(projB) shouldBe false

    ie.claimSubsume(a.index, b.index)
    ie.explore()

    projA.hasSuper(projB) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "πᵣ₁(πᵣ₁,ᵣ₂(A))" and "πᵣ₁(A)" to be equal (axiom 15)""" in {
    val ie = new IEngineCore()

    val a, b = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val projA = ie.getPI(a, Set(SBJ))
    val projB = ie.getPI(b, Set(SBJ))

    projA.hasSuper(projB) shouldBe false

    ie.claimSubsume(a.index, b.index)
    ie.explore()

    projA.hasSuper(projB) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "A⊂Bᵣ₁×Wᵣ₂" from "type(A)=(r₁,r₂)∧πᵣ₁(A)⊂B" (axiom 16)""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val projA = ie.getPI(a, Set(SBJ))
    val b = ie.new1DTerm().holder
    ie.claimSubsume(projA.index, b.index)

    val w = ie.getW(new Dimension(null)).holder
    val prodBW = ie.getCP(Set((b, SBJ), (w, OBJ)))

    ie.explore()

    a.hasSuper(prodBW) shouldBe true
    prodBW.hasSub(a) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "Aᵣ₁×Bᵣ₂≠∅" from "A≠∅∧B≠∅", and vice versa (axiom 17)""" in {
    val ie = new IEngineCore()

    {
      val a, b = ie.new1DTerm().holder
      val product = ie.getCP(Set((a, SBJ), (b, OBJ)))

      product.knownNE shouldBe false

      ie.claimNonEmpty(a.index)
      ie.claimNonEmpty(b.index)
      ie.explore()

      product.knownNE shouldBe true
    }

    {
      val a, b = ie.new1DTerm().holder
      val product = ie.getCP(Set((a, SBJ), (b, OBJ)))

      a.knownNE shouldBe false
      b.knownNE shouldBe false

      ie.claimNonEmpty(product.index)
      ie.explore()

      a.knownNE shouldBe true
      b.knownNE shouldBe true
    }

    ie.hasContradiction shouldBe false
  }

  it should """infer "Aᵣ₁×Cᵣ₂⊂Bᵣ₁×Dᵣ₂" from "A⊂B∧C⊂D" (axiom 18)""" in {
    val ie = new IEngineCore()

    val a, b, c, d = ie.new1DTerm().holder
    val prod1 = ie.getCP(Set((a, SBJ), (c, OBJ)))
    val prod2 = ie.getCP(Set((b, SBJ), (d, OBJ)))

    prod1.hasSuper(prod2) shouldBe false

    ie.claimSubsume(a.index, b.index)
    ie.claimSubsume(c.index, d.index)
    ie.explore()

    prod1.hasSuper(prod2) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer that "(Aᵣ₁×Bᵣ₂)×Cᵣ₃" and "Aᵣ₁×(Bᵣ₂×Cᵣ₃)" are equal (axiom 19)""" in {
    val ie = new IEngineCore()

    val a, b, c = ie.new1DTerm().holder

    val product1 = {
      val prodAB = ie.getCP(Set((a, SBJ), (b, OBJ)))

      ie.getCP(Set((prodAB, null), (c, IOBJ)))
    }
    val product2 = {
      val prodBC = ie.getCP(Set((b, OBJ), (c, IOBJ)))

      ie.getCP(Set((a, SBJ), (prodBC, null)))
    }

    ie.explore()

    product1.index shouldEqual product2.index
    ie.hasContradiction shouldBe false
  }

  it should """infer "B≠∅" from "πᵣ₁(Aᵣ₁×Bᵣ₂)=A" (axiom 20)""" in {
    val ie = new IEngineCore()

    val a, b = ie.new1DTerm().holder
    val product = ie.getCP(Set((a, SBJ), (b, OBJ)))
    val proj = ie.getPI(product, Set(SBJ))

    ie.explore()

    proj.index should not equal a.index

    ie.claimNonEmpty(b.index)
    ie.explore()

    proj.index shouldEqual a.index
    ie.hasContradiction shouldBe false
  }

  it should """infer that "(Aᵣ₁×Bᵣ₂)∩(Cᵣ₁×Dᵣ₂)" and "(A∩C)ᵣ₁×(B∩D)ᵣ₂" are equal (axiom 21)""" in {
    val ie = new IEngineCore()

    val a, b, c, d = ie.new1DTerm().holder

    val left = {
      val prodAB = ie.getCP(Set((a, SBJ), (b, OBJ)))
      val prodCD = ie.getCP(Set((c, SBJ), (d, OBJ)))
      ie.getIN(Set(prodAB, prodCD))
    }

    val right = {
      val intersectAC = ie.getIN(Set(a, c))
      val intersectBD = ie.getIN(Set(b, d))
      ie.getCP(Set((intersectAC, SBJ), (intersectBD, OBJ)))
    }

    ie.explore()

    left.index shouldEqual right.index
    ie.hasContradiction shouldBe false
  }

  it should """infer that "πᵣ₁((Aᵣ₁×Bᵣ₂)∩C)" and "A∩πᵣ₁((Wᵣ₁×Bᵣ₂)∩C)" are equal (axiom 22)""" in {
    val ie = new IEngineCore()

    val a, b = ie.new1DTerm().holder
    val c = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val w = ie.getW(new Dimension(null)).holder

    val left = {
      val prodAB = ie.getCP(Set((a, SBJ), (b, OBJ)))
      val intersect = ie.getIN(Set(prodAB, c))
      ie.getPI(intersect, Set(SBJ))
    }

    val right = {
      val prodWB = ie.getCP(Set((w, SBJ), (b, OBJ)))
      val intersect = ie.getIN(Set(prodWB, c))
      val proj = ie.getPI(intersect, Set(SBJ))
      ie.getIN(Set(a, proj))
    }

    ie.explore()

    left.index shouldEqual right.index
    ie.hasContradiction shouldBe false
  }

  it should """infer that "Bᵣ×qʳ{⊂}(A,B)" is a subset of "A" (axiom 23)""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val b = ie.new1DTerm().holder
    val quotient = ie.getFunc(FuncDIall, Seq(null, a, b), OBJ)
    val product = ie.getCP(Set((quotient, SBJ), (b, OBJ)))

    ie.explore()

    product.hasSuper(a) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "D⊂qʳ{⊂}(A,B)" from "B⊂C∧Cᵣ×D⊂A" (axiom 24)""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val b, c, d = ie.new1DTerm().holder
    val quotient = ie.getFunc(FuncDIall, Seq(null, a, b), OBJ)

    ie.explore()
    d.hasSuper(quotient) shouldBe false

    ie.claimSubsume(b.index, c.index)
    val product = ie.getCP(Set((c, OBJ), (d, SBJ)))
    ie.claimSubsume(product.index, a.index)

    ie.explore()
    d.hasSuper(quotient) shouldBe true
    ie.hasContradiction shouldBe false
  }


  it should """infer that "Bᵣ×qʳ{∥}(A,B)" and "A" are disjoint (axiom 25)""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val b = ie.new1DTerm().holder
    val quotient = ie.getFunc(FuncDIno, Seq(null, a, b), OBJ)
    val product = ie.getCP(Set((quotient, SBJ), (b, OBJ)))

    ie.explore()

    product.disjointTo(a) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "D ⊂ qʳ¹{∥}(A, B)" from "(B⊂C)∧(Cᵣ₁×Dᵣ₂∥A)∧(D⊂πᵣ₂(A))" (axiom 26 with the additional heuristic "D⊂πᵣ₂(A)")""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val b, c, d = ie.new1DTerm().holder
    val quotient = ie.getFunc(FuncDIno, Seq(null, a, b), OBJ)

    ie.explore()
    d.hasSuper(quotient) shouldBe false

    ie.claimSubsume(b.index, c.index)
    val product = ie.getCP(Set((c, OBJ), (d, SBJ)))
    ie.claimDisjoint(product.index, a.index)

    // Heuristic
    ie.claimSubsume(d.index, ie.getPI(a, Set(SBJ)).index)

    ie.explore()
    d.hasSuper(quotient) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer that "qʳ¹{∥}(A,B)" and "πᵣ₂((Bᵣ₁×Wᵣ₂)∩A)" are disjoint (axiom 27)""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val b = ie.new1DTerm().holder

    val left = ie.getFunc(FuncDIno, Seq(null, a, b), OBJ)
    val right = {
      val w = ie.getW(new Dimension(null)).holder
      val product = ie.getCP(Set((b, OBJ), (w, SBJ)))
      val intersect = ie.getIN(Set(product, a))

      ie.getPI(intersect, Set(SBJ))
    }

    ie.explore()

    (left disjointTo right) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer "C⊂qʳ¹{∥}(A,B)" from "C∥πᵣ₂((Bᵣ₁×Wᵣ₂)∩A) ∧ C⊂πᵣ₂(A)" (axiom 28, with the additional heuristic C⊂πᵣ₂(A))""" in {
    val ie = new IEngineCore()

    val a = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val b, c = ie.new1DTerm().holder
    val w = ie.getW(new Dimension(null)).holder
    val quotient = ie.getFunc(FuncDIno, Seq(null, a, b), OBJ)

    ie.explore()
    c.hasSuper(quotient) shouldBe false

    val product = ie.getCP(Set((b, OBJ), (w, SBJ)))
    val intersect = ie.getIN(Set(product, a))
    val proj = ie.getPI(intersect, Set(SBJ))
    ie.claimDisjoint(proj.index, c.index)

    ie.claimSubsume(c.index, ie.getPI(a, Set(SBJ)).index)

    ie.explore()
    c.hasSuper(quotient) shouldBe true
    ie.hasContradiction shouldBe false
  }

  it should """infer that "A∥A" and "A≠∅" results in a contradiction (axiom 29)""" in {
    val ie = new IEngineCore()
    ie.hasContradiction shouldBe false

    val a = ie.new1DTerm().holder
    ie.claimDisjoint(a.index, a.index)
    ie.claimNonEmpty(a.index)

    ie.explore()

    ie.hasContradiction shouldBe true
  }

  it should """infer "A=C" from "H=A×B=C×D∧A≠Ø∧B≠Ø"""" in {
    val ie = new IEngineCore()

    val a, c = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val b, d = ie.new1DTerm().holder
    val h = ie.newTerm(new Dimension(Set(SBJ, OBJ, IOBJ))).holder

    ie.claimCP(h.index, Set((a.index, null: SemRole), (b.index, IOBJ)))
    ie.claimCP(h.index, Set((c.index, null: SemRole), (d.index, IOBJ)))

    // Necessary for the engine to explore relationships between a and c
    ie.getPI(h, Set(SBJ, OBJ))

    ie.explore()
    a.index should not equal c.index

    ie.claimNonEmpty(a.index)

    ie.explore()
    a.index should not equal c.index

    ie.claimNonEmpty(b.index)
    ie.explore()
    a.index shouldEqual c.index
    ie.hasContradiction shouldBe false
  }

  it should """infer "Tom has an animal that Mary loves" from "Mary loves every dog" and "Tom has a dog", with the knowledge that "All dogs are animals" """ in {
    val ie = new IEngineCore()

    val mary, dog, tom, animal = ie.new1DTerm().holder
    val love, have = ie.newTerm(new Dimension(Set(SBJ, OBJ))).holder
    val w = ie.getW(new Dimension(null)).holder

    // Mary loves every dog
    ie.claimSubsume(
      dog.index,
      ie.getPI(
        ie.getIN(Set(
          love,
          ie.getCP(Set(
            (mary, SBJ),
            (w, OBJ)
          ))
        )),
        Set(OBJ)
      ).index
    )

    // Tom has a dog
    ie.claimNonEmpty(
      ie.getIN(Set(
        have,
        ie.getCP(Set((tom, SBJ), (dog, OBJ)))
      )).index
    )

    // Background knowledge: all dogs are animals
    ie.claimSubsume(dog.index, animal.index)

    // Tom has an animal that Mary loves
    val h = {
      val animalsThatMaryLoves =
        ie.getPI(
          ie.getIN(Set(
            love,
            ie.getCP(Set((mary, SBJ), (animal, OBJ)))
          )),
          Set(OBJ)
        )

      ie.getIN(Set(
        ie.getCP(Set((tom, SBJ), (animalsThatMaryLoves, OBJ))),
        have
      ))
    }

    ie.explore()

    h.knownNE shouldBe true
  }

  val customEmptySeq = new Seq[RuleArg] {
    override def length: Int = 0

    override def apply(idx: Int): RuleArg = ???

    override def iterator: Iterator[RuleArg] = Iterator.empty
  }

  it should "notify with registered callbacks about set non-emptiness" in {
    val ie = new IEngineCore()
    val termIndex = ie.new1DTerm()
    ie.claimNonEmpty(termIndex)
    ie.explore()

    // The notifications will be sent even if the inference happened before the callback was registered
    var flag = 0
    ie.ifNotEmpty(termIndex, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.term should be theSameInstanceAs termIndex
        flag += 1
      }))
    ie.explore()

    flag shouldEqual 1
  }

  it should "notify with registered callbacks about super-sets or subsets" in {
    val ie = new IEngineCore()

    val termIndex1, termIndex2 = ie.new1DTerm()
    val w = ie.getW(new Dimension(null))
    ie.claimSubsume(termIndex1, termIndex2)
    ie.explore()

    // The notifications will be sent even if the inference happened before the callback was registered
    var flag1W, flag1Self, flag12, flag2Self, flag21, flagSubsume = 0
    ie.foreachSuperset(termIndex1, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.subset should be theSameInstanceAs termIndex1
        p.superset match {
          case `w` => flag1W += 1
          case `termIndex1` => flag1Self += 1
          case `termIndex2` => flag12 += 1
          case _ => fail()
        }
      }))
    ie.foreachSubset(termIndex2, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.superset should be theSameInstanceAs termIndex2
        p.subset match {
          case `termIndex2` => flag2Self += 1
          case `termIndex1` => flag21 += 1
          case _ => fail()
        }
      }))
    ie.ifSubsume(termIndex1, termIndex2, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.subset should be theSameInstanceAs termIndex1
        p.superset should be theSameInstanceAs termIndex2
        flagSubsume += 1
      }))
    ie.explore()

    flag1W shouldEqual 1
    flag1Self shouldEqual 1
    flag12 shouldEqual 1
    flag2Self shouldEqual 1
    flag21 shouldEqual 1
    flagSubsume shouldEqual 1
  }

  it should "notify with registered callbacks about sets being disjoint" in {
    val ie = new IEngineCore()

    val termIndex1, termIndex2 = ie.new1DTerm()
    val w = ie.getW(new Dimension(null))

    ie.claimDisjoint(termIndex1, termIndex2)
    ie.explore()

    var flag1To2, flag2To1, flag1ToSelf, flag1And2, flag1AndSelf = 0

    ie.ifDisjoint(termIndex1, termIndex2, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredDisjoint(termIndex1, termIndex2)
        flag1And2 += 1
      }))

    ie.ifDisjoint(termIndex1, termIndex1, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredDisjoint(termIndex1, termIndex1)
        flag1AndSelf += 1
      }))

    ie.foreachDisjoint(termIndex1, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p match {
          case IEPredDisjoint(`termIndex1`, `termIndex2`) => flag1To2 += 1
          case IEPredDisjoint(`termIndex2`, `termIndex1`) => flag2To1 += 1
          case IEPredDisjoint(`termIndex1`, `termIndex1`) => flag1ToSelf += 1
          case _ => fail()
        }
      }))
    ie.explore()

    flag1To2 shouldEqual 1
    flag2To1 shouldEqual 1
    flag1And2 shouldEqual 1
    flag1ToSelf shouldEqual 0
    flag1AndSelf shouldEqual 0

    ie.claimDisjoint(termIndex1, termIndex1)
    ie.explore()

    flag1To2 shouldEqual 1
    flag2To1 shouldEqual 1
    flag1And2 shouldEqual 1
    flag1ToSelf shouldEqual 1
    flag1AndSelf shouldEqual 1
  }

  it should "notify with registered callbacks about sets being projected/a projection" in {
    val ie = new IEngineCore()

    val termIndex1 = ie.newTerm(new Dimension(Set(SBJ, OBJ, IOBJ)))
    val termIndex2 = ie.getPI(termIndex1.holder, Set(SBJ, OBJ)).index
    val termIndex3 = ie.new1DTerm()
    ie.claimPI(termIndex3, termIndex1, SBJ)

    var flag1To2, flag1To3, flag2To3, flag2From1, flag3From1, flag3From2 = 0

    ie.foreachMkPI(termIndex1, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.compt should be theSameInstanceAs termIndex1
        p.head match {
          case `termIndex2` =>
            p.compr shouldBe null
            flag1To2 += 1
          case `termIndex3` =>
            p.compr shouldEqual SBJ
            flag1To3 += 1
          case _ => fail()
        }
      }))

    ie.foreachMkPI(termIndex2, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredPI(termIndex3, termIndex2, SBJ)
        flag2To3 += 1
      }))

    ie.foreachIsPI(termIndex2, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredPI(termIndex2, termIndex1, null)
        flag2From1 += 1
      }))

    ie.foreachIsPI(termIndex3, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.head should be theSameInstanceAs termIndex3
        p.compr shouldEqual SBJ
        p.compt match {
          case `termIndex1` => flag3From1 += 1
          case `termIndex2` => flag3From2 += 1
          case _ => fail()
        }
      }))

    ie.explore()

    flag1To2 shouldEqual 1
    flag1To3 shouldEqual 1
    flag2To3 shouldEqual 1
    flag2From1 shouldEqual 1
    flag3From2 shouldEqual 1
  }

  it should "notify with registered callbacks about sets are involved in Cartesian products" in {
    val ie = new IEngineCore()

    val termIndex1, termIndex2, termIndex3 = ie.new1DTerm()
    val termIndex4 = ie.newTerm(new Dimension(Set(SBJ, OBJ)))

    val indexRoleSet12 = Set((termIndex1, SBJ), (termIndex2, OBJ))
    ie.claimCP(termIndex4, indexRoleSet12)

    val indexRoleSet123 = Set((termIndex1, SBJ), (termIndex2, OBJ), (termIndex3, IOBJ))
    val indexRoleSet34 = Set((termIndex3, IOBJ), (termIndex4, null: SemRole))

    val termIndex5 = ie.getCP(Set((termIndex4.holder, null), (termIndex3.holder, IOBJ))).index
    ie.explore()

    var
    flag1For4, flag1For5,
    flag2For4, flag2For5,
    flag3For5With12, flag3For5With4,
    flag4For5, flag4From12,
    flag5From123, flag5From34,
    flag12, flag123, flag34 = 0

    ie.foreachMkCP(termIndex1, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p match {
          case IEPredCP(`termIndex4`, `indexRoleSet12`) => flag1For4 += 1
          case IEPredCP(`termIndex5`, `indexRoleSet123`) => flag1For5 += 1
          case _ => fail()
        }
      }))

    // This one is identical to above
    ie.foreachMkCP(termIndex2, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p match {
          case IEPredCP(`termIndex4`, `indexRoleSet12`) => flag2For4 += 1
          case IEPredCP(`termIndex5`, `indexRoleSet123`) => flag2For5 += 1
          case _ => fail()
        }
      }))

    ie.foreachMkCP(termIndex3, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.head should be theSameInstanceAs termIndex5
        p.comp match {
          case `indexRoleSet123` => flag3For5With12 += 1
          case `indexRoleSet34` => flag3For5With4 += 1
          case _ => fail()
        }
      }))

    ie.foreachMkCP(termIndex4, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredCP(termIndex5, indexRoleSet34)
        flag4For5 += 1
      }))

    ie.foreachIsCP(termIndex4, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredCP(termIndex4, indexRoleSet12)
        flag4From12 += 1
      }))

    ie.foreachIsCP(termIndex5, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.head should be theSameInstanceAs termIndex5
        p.comp match {
          case `indexRoleSet123` => flag5From123 += 1
          case `indexRoleSet34` => flag5From34 += 1
          case _ => fail()
        }
      }))

    ie.forCPof(indexRoleSet12, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredCP(termIndex4, indexRoleSet12)
        flag12 += 1
      }))

    ie.forCPof(indexRoleSet123, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredCP(termIndex5, indexRoleSet123)
        flag123 += 1
      }))

    ie.forCPof(indexRoleSet34, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredCP(termIndex5, indexRoleSet34)
        flag34 += 1
      }))

    ie.explore()
    flag1For4 shouldEqual 1
    flag1For5 shouldEqual 1
    flag2For4 shouldEqual 1
    flag2For5 shouldEqual 1
    flag3For5With12 shouldEqual 1
    flag3For5With4 shouldEqual 1
    flag4For5 shouldEqual 1
    flag4From12 shouldEqual 1
    flag5From123 shouldEqual 1
    flag5From34 shouldEqual 1
    flag12 shouldEqual 1
    flag123 shouldEqual 1
    flag34 shouldEqual 1
  }

  it should "notify with registered callbacks about sets are involved in set intersections *when set dimensions are >=2 and parameter 'aux' is false*" in {
    val ie = new IEngineCore()

    val termIndex1, termIndex2 = ie.newTerm(new Dimension(Set(SBJ, OBJ)))

    val indexSet12 = Set(termIndex1, termIndex2)
    val termIndex3 = ie.getIN(indexSet12.map(_.holder)).index

    ie.explore()

    var flag1, flag2, flag3 = 0

    ie.foreachIsIN(termIndex3, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.head should be theSameInstanceAs termIndex3
        p.comp shouldEqual indexSet12
        flag3 += 1
      }))

    ie.foreachMkIN(termIndex1, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.head should be theSameInstanceAs termIndex3
        p.comp shouldEqual indexSet12
        flag1 += 1
      }))

    ie.foreachMkIN(termIndex2, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p.head should be theSameInstanceAs termIndex3
        p.comp shouldEqual indexSet12
        flag2 += 1
      }))

    ie.explore()

    flag1 shouldEqual 1
    flag2 shouldEqual 1
    flag3 shouldEqual 1
  }

  it should "notify with registered callbacks about custom set relations" in {
    object DummyRelation extends Relation {
      override def execute[T](ex: Executor, a: T, b: T): Unit = {
        // Nothing
      }
    }

    val ie = new IEngineCore()

    val termIndex1, termIndex2 = ie.new1DTerm()
    ie.claimRL(termIndex1, DummyRelation, termIndex2)
    ie.explore()

    var flag1, flag2, flag3 = 0

    ie.foreachARLX(termIndex1, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredRL(termIndex1, DummyRelation, termIndex2)
        flag1 += 1
      }))
    ie.foreachXRLB(termIndex2, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredRL(termIndex1, DummyRelation, termIndex2)
        flag2 += 1
      }))
    ie.ifRelation(termIndex1, termIndex2, customEmptySeq,
      RuleDo((i, p, args) => {
        i should be theSameInstanceAs ie
        args should be theSameInstanceAs customEmptySeq
        p shouldEqual IEPredRL(termIndex1, DummyRelation, termIndex2)
        flag3 += 1
      }))

    ie.explore()

    flag1 shouldEqual 1
    flag2 shouldEqual 1
    flag3 shouldEqual 1
  }

}
