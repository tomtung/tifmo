package tifmo.document

import tifmo.dcstree.{ Declarative, Relation, Executor, Selection }
import tifmo.inference._
import tifmo.onthefly.AEngine

case class SelAtLeast(cardinal: String) extends Selection {
  override def execute[T](ex: Executor, x: T): T = {
    (ex, x) match {
      case (ie: IEngineCore, tm: Term) =>
        val sel = ie.getFunc(FuncContravariance, Seq(null, tm), this)
        ie.claimNonEmpty(ie.getIN(Set(sel, tm)).index)
        sel.asInstanceOf[T]
      case (ae: AEngine, toProve: Function1[_, _]) =>
        ((d: Declarative) => {}).asInstanceOf[T]
    }
  }
}
