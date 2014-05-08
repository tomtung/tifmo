package tifmo.document

import tifmo.inference._

/**
 * `tms = Seq(B, A)` means that `B = F(param, A)`, such that
 * for each `A' ⊃ A`, if `B' = F(param, A')`, then `B' ⊂ B`
 */
trait FuncContravarianceLike extends IEFunction {
  def getMark(ie: IEngineCore, tms: Seq[TermIndex], param: Any): Any

  override def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
    super.applyFunc(ie, tms, param)
    tms match {
      case Seq(ft, t) =>
        val mark = getMark(ie, tms, param)
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
                  case _ => // Nothing
                }
              })
            )
          })
        )
    }
  }
}
