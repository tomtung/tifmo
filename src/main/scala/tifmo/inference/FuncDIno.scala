package tifmo.inference

import tifmo.dcstree.SemRole

/**
 * Division with QuantifierNO.
 *
 * `tms = Seq(H, A, B)`, `param = r` means that `H = qʳ{∥}(A,B)`.
 */
object FuncDIno extends IEFunction {

  def headDim(tms: Seq[Term], param: Any) = {
    val r = param.asInstanceOf[SemRole]
    Dimension(tms(1).dim.decrease(r))._1
  }

  override def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
    val r = param.asInstanceOf[SemRole]
    tms match {
      case Seq(h, a, b) =>
        val hrs = a.dim.decrease(r)
        val (hdim, hr) = Dimension(hrs)

        val pia = ie.getPI(a.holder, hrs) // π₋ᵣ(A)
        val acomp = {
          val tot = ie.getCP(Set((pia, hr), (ie.getW(b.dim).holder, r))) // π₋ᵣ(A) × Wᵣ
          ie.getFunc(FuncComplement, Seq(null, a.holder, tot), null) // Acomp = (π₋ᵣ(A) × Wᵣ) \ A
        }
        // H = qʳ{⊂}(Acomp, B) = qʳ{⊂}((π₋ᵣ(A) × Wᵣ) \ A, B)
        // This effectively encodes the following two axioms:
        // - Bᵣ × H ∥ A
        //   - which follows from "Bᵣ × H ⊂ Acomp"
        // - (B ⊂ C) ∧ (Cᵣ₁ × Dᵣ₂ ∥ A) ⇒ D ⊂ H
        //   - which follows from the similar-looking axiom for "qʳ{⊂}(A,B)" and "Cᵣ₁ × Dᵣ₂ ⊂ Acomp"
        // with the additional heuristic requirement:
        //   H ⊂ πᵣ₂(A)
        ie.claimFunc(FuncDIall, Seq(h, acomp.index, b), r, Debug_SimpleRuleTrace("FuncDIno", ie.getNewPredID()))

        val hcomp = {
          val bw = ie.getCP(Set((b.holder, r), (ie.getW(hdim).holder, hr))) // Bᵣ × W₋ᵣ
          ie.getPI(ie.getIN(Set(bw, a.holder)), hrs) // Hcomp = π₋ᵣ(A ∩ (Bᵣ × W₋ᵣ))
        }
        // H = π₋ᵣ(A) \ Hcomp = π₋ᵣ(A) \ π₋ᵣ(A ∩ (Bᵣ × W₋ᵣ))
        // This effectively encodes the following two axioms:
        // - H and Hcomp are disjoint
        // - C ∥ Hcomp ⇒ C ⊂ H
        // with the additional heuristic requirement:
        //   H ⊂ πᵣ₂(A)
        ie.claimFunc(FuncComplement, Seq(h, hcomp.index, pia.index), null, Debug_SimpleRuleTrace("FuncDIno", ie.getNewPredID()))

      case _ => throw new Exception("FuncDIno error!")
    }
  }
}
