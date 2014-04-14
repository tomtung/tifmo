package tifmo.inference

import tifmo.dcstree.SemRole
import RAConversion._

/**
 * Division with QuantifierALL.
 *
 * `tms = Seq(H, A, B)`, `param = r` means that `H = qʳ{⊂}(A,B)`
 */
object FuncDIall extends IEFunction {

  def headDim(tms: Seq[Term], param: Any) = {
    val r = param.asInstanceOf[SemRole]
    Dimension(tms(1).dim.decrease(r))._1
  }

  def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
    val r = param.asInstanceOf[SemRole]
    tms match {
      case Seq(h, a, b) =>
        val hrs = a.dim.decrease(r)
        // H ⊂ π₋ᵣ(A)
        // This tells the engine to keep an eye on projection π₋ᵣ(A)
        ie.claimSubsume(h, ie.getPI(a.holder, hrs).index, Debug_SimpleRuleTrace("FuncDIall", ie.getNewPredID()))

        val (_, hr) = Dimension(hrs)
        // Axiom: Bᵣ × H ⊂ A
        ie.claimSubsume(ie.getCP(Set((h.holder, hr), (b.holder, r))).index, a, Debug_SimpleRuleTrace("FuncDIall", ie.getNewPredID()))

        // Axiom: (B⊂C) ∧ (Cᵣ×D⊂A) ⇒ D ⊂ H
        // To encode this, we start from "Cᵣ×D⊂A", by checking each subset of A
        // See details in the implementation of rDI2, rDI1, and rDI0
        ie.foreachSubset(a, Seq(b, r, h), rDI2)

      case _ => throw new Exception("FuncDIall error!")
    }
  }
}

private[inference] object rDI2 extends RuleDo[IEPredSubsume] {
  def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
    // Following FuncDIall.applyFunc, we are now in the process of encoding "Cᵣ×D⊂A"
    // Here we only care about the subsets of A that are known to be cross products of other terms
    ie.foreachIsCP(pred.subset, args, rDI1)
  }
}

private[inference] object rDI1 extends RuleDo[IEPredCP] {
  def apply(ie: IEngineCore, pred: IEPredCP, args: Seq[RuleArg]) {
    args match {
      case Seq(RuleArg(b: TermIndex), RuleArg(r: SemRole), RuleArg(h: TermIndex)) =>
        // Following rDI2, we encode "Cᵣ×D⊂A" by looking for potential "C" with "pred.comp.find"
        // What's left in "pred.comp - ccomp" is effectively "D"
        for (ccomp @ (c, _) <- pred.comp.find(_._2 == r)) {
          // Finally if "C" is indeed a superset of "B", the premise of the axiom is met
          // We proceed to rDI0 and claim D ⊂ H
          ie.ifSubsume(b, c, Seq(pred.comp - ccomp, h), rDI0)
        }

      case _ => throw new Exception("rDI1 error!")
    }
  }
}

private[inference] object rDI0 extends RuleDo[IEPredSubsume] {
  def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {

    // Ugly hack to get around type erasure
    def isTermIndexSemRolePair(o: Any) = {
      o.isInstanceOf[(_, _)] && {
        val pair = o.asInstanceOf[(_, _)]
        pair._1.isInstanceOf[TermIndex] && pair._2.isInstanceOf[SemRole]
      }
    }

    args match {
      case Seq(RuleArg(set: Set[_]), RuleArg(h: TermIndex)) if set.forall(isTermIndexSemRolePair) =>
        val tc = set.asInstanceOf[Set[(TermIndex, SemRole)]]
        if (tc.size <= 1) {
          // If the dimensionality of D is <= 1, claim D ⊂ H directly
          val d = tc.head._1
          ie.claimSubsume(d, h, Debug_SimpleRuleTrace("FuncDIall", ie.getNewPredID()))
        } else {
          // Otherwise, first construct D through cross product before claiming D ⊂ H
          ie.constructCP(
            tc.map(y => (y._1.holder, y._2)), Seq.empty,
            (d: Term, _) => {
              ie.claimSubsume(d.index, h, Debug_SimpleRuleTrace("FuncDIall", ie.getNewPredID()))
            })
        }

      case _ => throw new Exception("rDI0 error!")
    }
  }
}
