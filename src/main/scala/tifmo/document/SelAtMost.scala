package tifmo.document

import tifmo.dcstree.{ Declarative, Executor, Selection }
import tifmo.inference._
import tifmo.onthefly.AEngine

case class SelAtMost(cardinal: String) extends Selection {
  override def execute[T](ex: Executor, x: T): T = {
    (ex, x) match {
      case (ie: IEngineCore, tm: Term) =>
        ie.getFunc(FuncSelAtMost, Seq(null, tm), this).asInstanceOf[T]
      case (ae: AEngine, toProve: Function1[_, _]) =>
        ((d: Declarative) => {}).asInstanceOf[T]
    }
  }
}

private[document] object FuncSelAtMost extends IEFunction with FuncCovarianceLike {

  override def headDim(tms: Seq[Term], param: Any): Dimension = tms(1).dim

  override def getMark(ie: IEngineCore, tms: Seq[TermIndex], param: Any): Any = (this, param)

  override def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
    tms match {
      case Seq(sel, tm) =>
        super.applyFunc(ie, tms, param)
        ie.claimNonEmpty(sel)
    }
  }

}
