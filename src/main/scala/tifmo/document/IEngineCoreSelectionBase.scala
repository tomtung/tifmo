package tifmo.document

import tifmo.dcstree.{ Declarative, Executor, Selection }
import tifmo.inference.{ Term, IEngineCore }
import tifmo.onthefly.AEngine

abstract class IEngineCoreSelectionBase extends Selection {

  protected def executeOnGerm(ie: IEngineCore, term: Term, result: Term) {

  }

  override def execute[T](ex: Executor, x: T): T = (ex, x) match {
    case (ie: IEngineCore, tm: Term) =>
      val result = ie.newTerm(tm.dim).holder
      executeOnGerm(ie, tm, result)
      result.asInstanceOf[T]
    case (ae: AEngine, toProve: Function1[_, _]) =>
      ((d: Declarative) => {}).asInstanceOf[T]
    case _ =>
      throw new Exception("unknown executor!!")
  }

}
