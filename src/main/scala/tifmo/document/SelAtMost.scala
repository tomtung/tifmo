package tifmo.document

import tifmo.dcstree.{ Declarative, Executor, Selection }
import tifmo.inference._
import tifmo.onthefly.AEngine

case class SelAtMost(cardinal: String) extends Selection {
  override def execute[T](ex: Executor, x: T): T = {
    (ex, x) match {
      case (ie: IEngineCore, tm: Term) =>
        val sel = ie.getFunc(FuncCovariance, Seq(null, tm), cardinal)
        ie.claimNonEmpty(sel.index)
        sel.asInstanceOf[T]
      case (ae: AEngine, toProve: Function1[_, _]) =>
        ((d: Declarative) => {}).asInstanceOf[T]
    }
  }
}
