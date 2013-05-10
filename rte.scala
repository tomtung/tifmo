
import tifmo.knowledge.EnWord
import tifmo.proc.mkSTreeEnglish
import tifmo.stree.InferMgr
import tifmo.stree.Align

import scala.collection.mutable

def confidence(algn: Align, cache: mutable.Map[Set[Set[EnWord]], Double]) = {
	
	val cws = algn.clue.src.init.map(_.term.word.asInstanceOf[EnWord]).toSet
	val tws = if (algn.soft) {
			algn.tp.src.init.map(_.term.word.asInstanceOf[EnWord]).toSet -- cws
		} else {
			algn.tp.src.map(_.term.word.asInstanceOf[EnWord]).toSet -- cws
		}
	val hws = if (algn.soft) {
			algn.hp.src.init.map(_.term.word.asInstanceOf[EnWord]).toSet -- cws
		} else {
			algn.hp.src.map(_.term.word.asInstanceOf[EnWord]).toSet -- cws
		}
	if (tws.isEmpty || hws.isEmpty) {
		
		0.3 + 0.4 / cws.size
		
	} else if (hws.forall(x => tws.exists(y => EnWord.judgeSynonym(x, y)))) {
		
		0.8
		
	} else if (tws.forall(x => hws.exists(y => EnWord.judgeSynonym(x, y)))) {
		
		0.1 + 1.0 / (3 + hws.size - tws.size)
		
	} else if (tws.size <= 3 && hws.size <= 3) {
		
		if (cache.contains(Set(tws, hws))) {
			cache(Set(tws, hws))
		} else {
			val cossim = EnWord.cossim(tws, hws)
			val tmp = 1.0 / (1.0 - math.log(cossim))
			cache(Set(tws, hws)) = tmp
			tmp
		}
		
	} else {
		0.0
	}
}

val input_xml = "input/RTE_dev.xml"

val f = xml.XML.loadFile(input_xml)
for (p <- (f \ "pair")) {
	
	val traw = (p \ "t").text
	val tstree = mkSTreeEnglish(traw)
	
	val hraw = (p \ "h").text
	val hstree = mkSTreeEnglish(hraw)
	
	println("text: ")
	println(tstree)
	println("---")
	println("hypo: ")
	println(hstree)
	println("----")
	
	val imgr = new InferMgr(hstree)
	imgr.addPremise(tstree)
	
	val twords = tstree.streeNodeList.map(_.word).toSet
	val hwords = hstree.streeNodeList.map(_.word).toSet
	
	var lap = 0
	for (hwpre <- hwords; hw = hwpre.asInstanceOf[EnWord]) {
		var flag = false
		for (twpre <- twords; tw = twpre.asInstanceOf[EnWord]) {
			if (hw.lex == tw.lex) {
				flag = true
			} else {
				if (EnWord.judgeSynonym(hw, tw)) {
					flag = true
					println("Synonym: " + hw + " " + tw)
					imgr.addSynonym(hw, tw)
				}
				if (EnWord.judgeHypernym(hw, tw)) {
					println("Hypernym: " + hw + " -> " + tw)
					imgr.addHypernym(hw, tw)
				}
				if (EnWord.judgeHypernym(tw, hw)) {
					println("Hypernym: " + tw + " -> " + hw)
					imgr.addHypernym(tw, hw)
				}
				if (EnWord.judgeAntonym(tw, hw)) {
					println("Antonym: " + tw + " <> " + hw)
					imgr.addAntonym(tw, hw)
				}
			}
			
		}
		if (flag) lap += 1
	}
println("# words of H: " + hwords.size)
println("# words of H syn to T: " + lap)
	
	val cache = mutable.Map.empty[Set[Set[EnWord]], Double]
	imgr.trace(confidence(_, cache), 0.2)
	println("--------------")
}
