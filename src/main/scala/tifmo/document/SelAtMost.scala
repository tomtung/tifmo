package tifmo.document

import tifmo.dcstree.{Declarative, Relation, Executor, Selection}
import tifmo.inference._
import tifmo.onthefly.AEngine

case class SelAtMost(cardinal: String) extends Selection {

	// TODO extract a trait/baseclass "CovarianceSelection"?

	import SelAtMost._

	override def execute[T](ex: Executor, x: T): T = {
		(ex, x) match {
			case (ie: IEngineCore, tm: Term) =>
				ie.getFunc(SelAtMostFunc, Seq(null, tm), cardinal).asInstanceOf[T]
			case (ae: AEngine, toProve: Function1[_, _]) =>
				((d: Declarative) => {}).asInstanceOf[T]
		}
	}
}

object SelAtMost {

	private[document] object SelAtMostFunc extends IEFunction {
		override def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any): Unit = {
			tms match {
				case Seq(sel, base) =>
					val cardinal = param.asInstanceOf[String]
					val selTerm = sel.holder
					ie.claimNonEmpty(sel)
					ie.claimRL(sel, RelAtMost(cardinal), base)
					ie.foreachSubset(base, Seq.empty,
						RuleDo((ie, p, _) => {
							val subsetBase = p.subset
							ie.foreachXRLB(subsetBase, Seq.empty,
								RuleDo((ie, p, _) => {
									p.rl match {
										case RelAtMost(`cardinal`) =>
											val subsetSel = p.a
											ie.claimSubsume(subsetSel, selTerm.index)
									}
								})
							)
						})
					)
			}
		}

		override def headDim(tms: Seq[Term], param: Any): Dimension = tms(1).dim
	}

	private[document] case class RelAtMost(cardinal: String) extends Relation {
		override def execute[T](ex: Executor, sel: T, base: T): Unit = {
			// This relation is just a marker, do nothing
		}
	}
}
