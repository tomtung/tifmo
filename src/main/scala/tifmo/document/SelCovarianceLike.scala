package tifmo.document

import tifmo.inference.{ IEPredRL, RuleDo, Term, IEngineCore }

trait SelCovarianceLike extends IEngineCoreSelectionBase {

  protected def mark: Any = this

  override protected def executeOnGerm(ie: IEngineCore, term: Term, result: Term): Unit = {
    super.executeOnGerm(ie, term, result)
    ie.claimRL(term.index, RelMark(mark), result.index)
    ie.foreachSubset(term.index, Seq.empty, RuleDo((ie, pred, _) => {
      ie.foreachARLX(pred.subset, Seq.empty, RuleDo((ie, pred, _) => {
        pred match {
          case IEPredRL(_, RelMark(m), xTermSubSel) if m == this.mark =>
            ie.claimSubsume(xTermSubSel, result.index)
          case _ => // Do nothing
        }
      }))
    }))
  }

}
