package tifmo.document

import tifmo.inference._

/**
 * `tms = Seq(B, A)` means that `B = F(param, A)`, such that
 * for each `A' ⊂ A`, if `B' = F(param, A')`, then `B' ⊂ B`
 */
trait FuncCovarianceLike extends IEFunction {

  def getMark(ie: IEngineCore, tms: Seq[TermIndex], param: Any): Any

  override def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
    tms match {
      case Seq(ft, t) =>
        val mark = (this, param)
        ie.claimRL(ft, RelMark(mark), t)
        ie.foreachSubset(t, Seq.empty,
          RuleDo((ie, p, _) => {
            val tsub = p.subset
            ie.foreachXRLB(tsub, Seq.empty,
              RuleDo((ie, p, _) => {
                p.rl match {
                  case RelMark(`mark`) =>
                    val ftsub = p.a
                    ie.claimSubsume(ftsub, ft)
                }
              })
            )
          })
        )
    }
  }
}
