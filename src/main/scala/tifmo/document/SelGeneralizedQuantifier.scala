package tifmo.document

import tifmo.dcstree.{Declarative, Executor, Selection}
import tifmo.inference._
import tifmo.onthefly.AEngine

case class SelGeneralizedQuantifier(typeName: GeneralizedQuantifier) extends Selection {
	override def execute[T](ex: Executor, x: T): T = {
		(ex, x) match {
			case (ie: IEngineCore, tm: Term) =>
				ie.getFunc(SelGeneralQuantifierFunc, Seq(null, tm), typeName).asInstanceOf[T]
			case (ae: AEngine, toProve: Function1[_, _]) =>
				((d: Declarative) => {}).asInstanceOf[T]
			case _ =>
				throw new Exception("unknown executor!!")
		}
	}

	private[document] object SelGeneralQuantifierFunc extends IEFunction {
		override def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			tms match {
				case Seq(h, a) =>
					ie.claimSubsume(h, a)
					ie.claimNonEmpty(h) // Assumption: both a and h are not empty
			}
		}

		override def headDim(tms: Seq[Term], param: Any): Dimension = tms(1).dim
	}

}
