
import tifmo.knowledge.EnWord
import tifmo.proc.mkSTreeEnglish
import tifmo.stree.InferMgr
import tifmo.stree.Align
import tifmo.stree.PI

import scala.collection.mutable

def confidence(algn: Align, 
		acache: mutable.Map[Set[EnWord], (mutable.Map[String, Long], Double)], 
		bcache: mutable.Map[Set[EnWord], (mutable.Map[String, Long], Double)]) = {
	
	val cws = algn.clue.src.init.map(_.term.word.asInstanceOf[EnWord]).toSet
	def trimHead(x: List[PI]) = {
		x.map(_.term.word.asInstanceOf[EnWord]).dropWhile(y => cws.exists(EnWord.judgeSynonym(y, _))).toSet
	}
	val tws = if (algn.soft && EnWord.judgeSynonym(algn.tp.src.last.term.word.asInstanceOf[EnWord], algn.hp.src.last.term.word.asInstanceOf[EnWord])) {
			trimHead(algn.tp.src.init)
		} else {
			trimHead(algn.tp.src)
		}
	val hws = if (algn.soft) {
			trimHead(algn.hp.src.init)
		} else {
			trimHead(algn.hp.src)
		}
	if (tws.isEmpty || hws.isEmpty) {
		
		0.3 + 0.4 / (cws.size + hws.size)
		
	} else if (hws.forall(x => tws.exists(EnWord.judgeSynonym(x, _)))) {
		
		0.8
		
	} else if (tws.forall(x => hws.exists(EnWord.judgeSynonym(x, _)))) {
		
		0.1 + 1.0 / (3 + hws.size - tws.size)
		
	} else if (tws.size <= 3 && hws.size <= 3) {
		
		val cossim = EnWord.cossim(tws, hws, acache, bcache)
		1.0 / (1.0 - math.log10(cossim))
		
	} else {
		0.0
	}
}

val input_xml = "input/RTE_dev.xml"

val f = xml.XML.loadFile(input_xml)
for (p <- (f \ "pair")) {
	
	val traw = (p \ "t").text.trim
	val tstree = mkSTreeEnglish(traw)
	
	val hraw = (p \ "h").text.trim
	val hstree = mkSTreeEnglish(hraw)
	
	println("text: ")
	println(traw)
	println("---")
	println("hypo: ")
	println(hraw)
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
	
	val acache = mutable.Map.empty[Set[EnWord], (mutable.Map[String, Long], Double)]
	val bcache = mutable.Map.empty[Set[EnWord], (mutable.Map[String, Long], Double)]
	imgr.trace(confidence(_, acache, bcache), 0.2)
	println("--------------")
}
