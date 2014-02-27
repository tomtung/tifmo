package tifmo

import dcstree.SemRole
import dcstree.WordBase
import dcstree.QuantifierALL
import dcstree.QuantifierNO
import dcstree.Denotation
import dcstree.DenotationW
import dcstree.DenotationWordSign
import dcstree.DenotationIN
import dcstree.DenotationCP
import dcstree.DenotationPI
import dcstree.DenotationDI
import dcstree.DenotationSelection
import dcstree.Statement
import dcstree.StatementNotEmpty
import dcstree.StatementSubsume
import dcstree.StatementDisjoint
import dcstree.StatementRelation

import scala.collection.mutable
import scala.util.Sorting

package inference {
	
	class IEngine extends IEngineCore {
		
		// get term: 
		private[this] val wdrs = mutable.Map.empty[WordBase, Set[SemRole]]
		private[this] val wdtm = mutable.Map.empty[WordBase, Term]
		
		private[this] def getWordSign(roles: Set[SemRole], word: WordBase, sign: Boolean) = {
			assert(!word.isStopWord)
			
			def updatewdtm(roles: Set[SemRole], word: WordBase) = {
				wdrs(word) = roles
				val ret = newTerm(Dimension(roles)._1).holder
				if (word.isNamedEntity) claimFunc(FuncSingle, Seq(ret.index), ret.dim, Debug_SimpleRuleTrace("Named Entity", getNewPredID()))
				if (roles.size >= 2) for (r <- roles) getPI(ret, Set(r))
				wdtm(word) = ret
				ret
			}
			
			val pre = if (!wdrs.contains(word)) {
				updatewdtm(roles, word)
			} else {
				val rs = wdrs(word)
				val (ers, etm) = if (!roles.subsetOf(rs)) {
					val nrs = rs ++ roles
					val bak = wdtm(word)
					val nwd = updatewdtm(nrs, word)
					claimPI(bak.index, nwd.index, Dimension(rs)._2, Debug_SimpleRuleTrace("Word Role Ext", getNewPredID()))
					(nrs, nwd)
				} else {
					(rs, wdtm(word))
				}
				if (ers == roles) etm else getPI(etm, roles)
			}
			
			if (sign) pre else getFunc(FuncNegation, Seq(null, pre), null)
		}
		
		private[this] val dcache = mutable.Map.empty[Denotation, Term]
		/**
		 * Get an inference engine term from a denotation.
		 */
		def getTerm(denotation: Denotation) = {
			def recur(x: Denotation): Term = dcache.getOrElseUpdate(x, x match {
				case DenotationW(rs) => getW(Dimension(rs)._1).holder
				case DenotationWordSign(rs, wd, sgn) => getWordSign(rs, wd, sgn)
				case DenotationIN(x) => getIN(x.map(recur(_)))
				case DenotationCP(x) => {
					if (x.size <= 1) recur(x.head._1) else getCP(x.map(y => (recur(y._1), y._2)))
				}
				case DenotationPI(x, rs) => getPI(recur(x), rs)
				case DenotationDI(qt, x, y, r) => qt match {
					case QuantifierALL => getFunc(FuncDIall, Seq(null, recur(x), recur(y)), r)
					case QuantifierNO => getFunc(FuncDIno, Seq(null, recur(x), recur(y)), r)
				}
				case DenotationSelection(sel, x) => sel.execute[Term](this, recur(x))
			})
			val ret = recur(denotation)
			explore()
			ret
		}
		
		/**
		 * Add a statement as premise.
		 */
		def claimStatement(statement: Statement) {
			statement match {
				case StatementNotEmpty(x) => {
					claimNonEmpty(getTerm(x).index)
				}
				case StatementSubsume(sub, sup) => {
					val (xsub, xsup) = (getTerm(sub), getTerm(sup))
					claimSubsume(xsub.index, xsup.index)
				}
				case StatementDisjoint(a, b) => {
					val (xa, xb) = (getTerm(a), getTerm(b))
					claimDisjoint(xa.index, xb.index)
				}
				case StatementRelation(rel, a, b) => {
					val (xa, xb) = (getTerm(a), getTerm(b))
					claimRL(xa.index, rel, xb.index)
				}
			}
			explore()
		}
		
		/**
		 * Check if a statement is proven.
		 */
		def checkStatement(statement: Statement) = {
			statement match {
				case StatementNotEmpty(x) => {
					getTerm(x).knownNE
				}
				case StatementSubsume(sub, sup) => {
					val (xsub, xsup) = (getTerm(sub), getTerm(sup))
					xsub.hasSuper(xsup)
				}
				case StatementDisjoint(a, b) => {
					val (xa, xb) = (getTerm(a), getTerm(b))
					xa.disjointTo(xb)
				}
				case StatementRelation(rel, a, b) => {
					val (xa, xb) = (getTerm(a), getTerm(b))
					xa.hasARLX(rel, xb)
				}
			}
		}
		
		/**
		 * Claim a subsumption relation between two words.
		 */
		def subsume(sub: WordBase, sup: WordBase) {
			if (wdrs.contains(sub) && wdrs.contains(sup)) {
				val crs = wdrs(sub) intersect wdrs(sup)
				if (!crs.isEmpty) {
					claimSubsume(getWordSign(crs, sub, true).index, getWordSign(crs, sup, true).index, Debug_SimpleRuleTrace("word subsume", getNewPredID()))
				}
			}
		}
		
		/**
		 * Claim a disjoint relation between two words.
		 */
		def disjoint(a: WordBase, b: WordBase) {
			if (wdrs.contains(a) && wdrs.contains(b)) {
				val crs = wdrs(a) intersect wdrs(b)
				if (!crs.isEmpty) {
					claimDisjoint(getWordSign(crs, a, true).index, getWordSign(crs, b, true).index, Debug_SimpleRuleTrace("word disjoint", getNewPredID()))
				}
			}
		}
		
		// dump & load: 
		/**
		 * Dump a serializable record of current status.
		 */
		def dump() = {
			explore()
			
			val allws = WPool.values.toSet
			val alltms = allws.flatMap(_.subSets)
			val tmmap = (for (x <- alltms) yield {
				(x, new TermIndex(x.dim))
			}).toMap
			
			val preds = mutable.Set.empty[IEPred]
			val guards = mutable.Set.empty[Guard[_ <: IEPred]]
			for (tm <- alltms) {
				
				tm.kne.foreach(preds += _)
				tm.assub.foreach(preds += _)
				tm.djts.foreach(preds += _)
				tm.iscps.foreach(preds += _)
				tm.ispis.foreach(preds += _)
				tm.isins.foreach(preds += _)
				tm.funcs.foreach(preds += _)
				tm.asarl.foreach(preds += _)
				
				tm.neTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.subTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.superTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.djtTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.iscpTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.mkcpTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.ispiTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.mkpiTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.isinTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.mkinTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.arlTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
				tm.rlbTriggers.foreach(x => guards += x.guard.dumpMe(tmmap))
			}
			
			val stpreds = preds.toArray
			Sorting.quickSort[IEPred](stpreds)
			stpreds.foreach(_.debug_trace.assertValid())
			
			new IEDump(
				allws.map(tmmap(_)), 
				stpreds.toList.map(_.dumpMe(tmmap)), 
				guards.toList, 
				wdtm.mapValues(x => tmmap(x.index)).toMap, 
				wdrs.toMap, 
				dcache.mapValues(x => tmmap(x.index)).toMap
			)
		}
		
		/**
		 * Load status(es).
		 */
		def load(dps: IEDump*) {
			for (dp <- dps) {
				for (pred <- dp.preds) {
					val ck = pred.apply(Debug_SimpleRuleTrace("load", getNewPredID()))
					assert(ck)
				}
				for (guard <- dp.guards) {
					val ck = guard.locate(null)
					assert(ck)
					guard.setDis()
				}
			}
			val mgdps = if (W == null && !dps.isEmpty) {
				val dp = dps.head
				for (w <- dp.ws) {
					if (w.dim.size == 1) W = w
					WPool(w.dim) = w
					w.setwflag()
				}
				wdtm ++= dp.wdtm.mapValues(_.holder)
				wdrs ++= dp.wdrs
				dcache ++= dp.dcache.mapValues(_.holder)
				dps.tail
			} else {
				dps
			}
			
			for (dp <- mgdps) {
				for (x <- dp.ws) {
					val tm = getW(x.dim)
					claimSubsume(x, tm, Debug_SimpleRuleTrace("load W", getNewPredID())) 
					claimSubsume(tm, x, Debug_SimpleRuleTrace("load W", getNewPredID()))
				}
				for ((k, x) <- dp.wdtm) {
					val tm = getWordSign(dp.wdrs(k), k, true).index
					claimSubsume(x, tm, Debug_SimpleRuleTrace("load const", getNewPredID()))
					claimSubsume(tm, x, Debug_SimpleRuleTrace("load const", getNewPredID()))
				}
				dcache ++= dp.dcache.mapValues(_.holder)
			}
			
			explore()
		}
		
	}
}
