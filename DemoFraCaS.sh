#!/bin/sh
SCALA=scala
CORENLP_VERSION=stanford-corenlp-full-2014-01-04

JAVA_OPTS=-Xmx2g
export JAVA_OPTS

CLASSPATH_EN=lib/*:lib/en/*:lib/en/$CORENLP_VERSION/*

exec $SCALA -classpath $CLASSPATH_EN "$0" "$@"
!#

if (args.length != 1) {
	println("USAGE: DemoFraCaS.sh fracas_xml")
	sys.exit()
}

import tifmo.inference.IEngine
import tifmo.main.en.parse

val fracas_xml = args(0)

val f = xml.XML.loadFile(fracas_xml)

for (p <- (f \ "problem")) {
	
	val id = (p \ "@id").text
	
	val fracas_answer = (p \ "@fracas_answer").text
	
	if (fracas_answer == "undef") {
		
		println(id + "," + "undef,ignore")
		
	} else {
		
		val t = (p \ "p").map(_.text.trim).mkString("", " ", "")
		val h = (p \ "h").text.trim
		
		val (tdoc, hdoc) = parse(t, h)
		
		val prem = tdoc.makeDeclaratives.flatMap(_.toStatements)
		val hypo = hdoc.makeDeclaratives.flatMap(_.toStatements)
		
		val ie = new IEngine
		
		prem.foreach(ie.claimStatement(_))
		hypo.foreach(ie.checkStatement(_))
		
		if (hypo.forall(ie.checkStatement(_))) {
			
			println(id + "," + fracas_answer + ",yes")
			
		} else {
			
			// negate hdoc
			for (n <- hdoc.allRootNodes) {
				n.rootNeg = !n.rootNeg
			}
			val nhypo = hdoc.makeDeclaratives.flatMap(_.toStatements)
			
			val nie = new IEngine
			
			prem.foreach(nie.claimStatement(_))
			nhypo.foreach(nie.checkStatement(_))
			
			if (nhypo.forall(nie.checkStatement(_))) {
				
				println(id + "," + fracas_answer + ",no")
				
			} else {
				
				println(id + "," + fracas_answer + ",unknown")
				
			}
			
		}
		
	}
	
}




