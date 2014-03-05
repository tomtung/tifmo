#!/bin/sh
SCALA=scala
CORENLP_VERSION=stanford-corenlp-full-2014-01-04

JAVA_OPTS=-Xmx2g
export JAVA_OPTS

CLASSPATH_EN=lib/*:lib/en/*:lib/en/$CORENLP_VERSION/*

exec $SCALA -classpath $CLASSPATH_EN "$0" "$@"
!#

if (args.length != 1) {
	println("USAGE: DemoRTE.sh rte_xml")
	sys.exit()
}

import tifmo.dcstree.SemRole
import tifmo.dcstree.Statement
import tifmo.inference.IEngine
import tifmo.onthefly.AEngine
import tifmo.onthefly.alignPaths
import tifmo.onthefly.PathAlignment
import tifmo.main.en.normalize
import tifmo.main.en.parse
import tifmo.main.en.EnResources
import tifmo.main.en.EnWord
import tifmo.main.en.TIME
import tifmo.main.en.EnSimilarityMikolov13

import scala.collection.mutable

val rte_xml = args(0)

val f = xml.XML.loadFile(rte_xml)

var counter = 0
for (p <- (f \ "pair")) {
	
	val id = (p \ "@id").text
	
	val gold_label = if (
		(p \ "@value").text == "TRUE" 
			|| (p \ "@entailment").text == "ENTAILMENT" 
			|| (p \ "@entailment").text == "YES"
	) {
		"Y"
	} else {
		"N"
	}
	
	val t = (p \ "t").text.trim
	val h = (p \ "h").text.trim
	val (tdoc, hdoc) = parse(normalize(t), normalize(h))
	
	val prem = tdoc.makeDeclaratives
	val hypo = hdoc.makeDeclaratives
	
	val ie = new IEngine
	
	prem.flatMap(_.toStatements).foreach(ie.claimStatement(_))
	hypo.flatMap(_.toStatements).foreach(ie.checkStatement(_))
	
	// add linguistic knowledge
	val res = new EnResources
	val words = tdoc.allContentWords[EnWord] ++ hdoc.allContentWords[EnWord]
	for (s <- words.subsets(2)) {
		val a = s.head
		val b = (s - a).head
		if (res.synonym(a, b)) {
			ie.subsume(a, b)
			ie.subsume(b, a)
		} else if (Set(a.mypos, b.mypos).subsetOf(Set("J", "R")) && res.antonym(a, b)) {
			ie.disjoint(a, b)
		} else {
			if (res.hyponym(a, b) && !b.isNamedEntity) {
				ie.subsume(a, b)
			}
			if (res.hyponym(b, a) && !a.isNamedEntity) {
				ie.subsume(b, a)
			}
		}
	}
	
	if (hypo.flatMap(_.toStatements).forall(ie.checkStatement(_))) {
		
		println(id + "," + counter + "," + gold_label + ",Y")
		
	} else {
		
		val ae = new AEngine(prem)
		hypo.foreach(ae.addGoal(_))
		val alignedpaths = alignPaths(ae.allAssumptions, ie)
		
		val sim = new EnSimilarityMikolov13(res)
		
		val fil = mutable.Set.empty[PathAlignment]
		for (x @ PathAlignment(psub, psup) <- alignedpaths) {
			// evaluate path alignments
			
			if (psup.rnrs.exists(x => x._1 == TIME || x._3 == TIME) 
						&& !psub.rnrs.exists(x => x._1 == TIME || x._3 == TIME)) {
				// time role unmatch, filtered.
			} else {
				
				val wsub = psub.rnrs.map(x => x._2.token.getWord.asInstanceOf[EnWord]).filter(!_.isStopWord)
				val wsup = psup.rnrs.map(x => x._2.token.getWord.asInstanceOf[EnWord]).filter(!_.isStopWord)
				
				if (wsub.isEmpty || wsup.isEmpty) {
					// filtered.
				} else if (wsup.exists(x => (x.isNamedEntity || x.mypos == "D") 
						&& !wsub.exists(y => res.synonym(y, x) || res.hyponym(y, x)))) {
					// time or named entity unmatch, filtered.
				} else {
					// phrase similarity
					if (sim.similarity(wsub, wsup) > 0.1) {
						fil += x
					}
				}
			}
		}
		
		// add on-the-fly knowledge
		fil.flatMap(_.toOnTheFly(ie).toStatements).foreach(ie.claimStatement(_))
		
		if (hypo.flatMap(_.toStatements).forall(ie.checkStatement(_))) {
			
			println(id + "," + counter + "," + gold_label + ",Y")
			
		} else {
			
			println(id + "," + counter + "," + gold_label + ",N")
			
		}
	}
	
	counter += 1
}

