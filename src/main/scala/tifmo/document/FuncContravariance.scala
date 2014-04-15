package tifmo.document

import tifmo.inference._

/**
 * `tms = Seq(B, A)` means that `B = F(param, A)`, such that
 * for each `A' ⊃ A`, if `B' = F(param, A')`, then `B' ⊂ B`
 *
 * Note: for the moment we assume A and B to have the same dimension.
 * Could introduce an extra parameter if necessary.
 */
object FuncContravariance extends IEFunction {
  override def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
    tms match {
      case Seq(ft, t) =>
        val mark = (this, param)
        ie.claimRL(ft, RelMark(mark), t)
        ie.foreachSuperset(t, Seq.empty,
          RuleDo((ie, p, _) => {
            val tsup = p.superset
            ie.foreachXRLB(tsup, Seq.empty,
              RuleDo((ie, p, _) => {
                p.rl match {
                  case RelMark(`mark`) =>
                    val ftsup = p.a
                    ie.claimSubsume(ftsup, ft)
                }
              })
            )
          })
        )
    }
  }

  override def headDim(tms: Seq[Term], param: Any): Dimension = tms(1).dim
}
