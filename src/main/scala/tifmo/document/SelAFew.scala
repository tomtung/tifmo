package tifmo.document

import tifmo.dcstree.{ Declarative, Executor, Selection }
import tifmo.inference._
import tifmo.onthefly.AEngine

case object SelAFew extends Selection {

  override def execute[T](ex: Executor, x: T): T = {
    (ex, x) match {
      case (ie: IEngineCore, tm: Term) =>
        ie.getFunc(FuncSelAFew, Seq(null, tm), null).asInstanceOf[T]
      case (ae: AEngine, toProve: Function1[_, _]) =>
        ((d: Declarative) => {}).asInstanceOf[T]
      case _ =>
        throw new Exception("unknown executor!!")
    }
  }

}

private[document] object FuncSelAFew extends IEFunction with FuncContravarianceLike {
  override def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
    tms match {
      case Seq(h, a) =>
        super.applyFunc(ie, tms, param)
        ie.claimSubsume(h, a)
        ie.claimNonEmpty(h)
    }
  }

  override def headDim(tms: Seq[Term], param: Any): Dimension = tms(1).dim

  override def getMark(ie: IEngineCore, tms: Seq[TermIndex], param: Any): Any = this
}
