package tifmo.document

import tifmo.inference.{ Term, IEngineCore }

trait SelWithNonEmptyIntersection extends IEngineCoreSelectionBase {
  override protected def executeOnGerm(ie: IEngineCore, term: Term, result: Term): Unit = {
    super.executeOnGerm(ie, term, result)
    ie.claimNonEmpty(ie.getIN(Set(term, result)).index)
  }
}
