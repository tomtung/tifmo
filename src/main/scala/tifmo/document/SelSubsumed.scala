package tifmo.document

import tifmo.inference.{ IEngineCore, Term }

trait SelSubsumed extends IEngineCoreSelectionBase {
  override protected def executeOnGerm(ie: IEngineCore, term: Term, result: Term): Unit = {
    super.executeOnGerm(ie, term, result)
    ie.claimSubsume(result.asInstanceOf[Term].index, term.index)
  }
}
