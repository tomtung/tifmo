package tifmo.document

import tifmo.inference.{ IEPredRL, RuleDo, Term, IEngineCore }

trait SelContravarianceLike extends IEngineCoreSelectionBase {

  protected def mark: Any = this

  override protected def executeOnGerm(ie: IEngineCore, term: Term, result: Term): Unit = {
    super.executeOnGerm(ie, term, result)
    ie.claimRL(term.index, RelMark(mark), result.index)
    ie.foreachSuperset(term.index, Seq.empty, RuleDo((ie, pred, _) => {
      ie.foreachARLX(pred.superset, Seq.empty, RuleDo((ie, pred, _) => {
        pred match {
          case IEPredRL(_, RelMark(m), xTermSupSel) if m == this.mark =>
            ie.claimSubsume(xTermSupSel, result.index)
          case _ => // Do nothing
        }
      }))
    }))
  }

}
