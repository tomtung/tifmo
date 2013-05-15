
import java.io.FileWriter
import java.io.BufferedWriter

import tifmo.knowledge.StopWords
import tifmo.knowledge.EnWord
import tifmo.proc.preProcEnglish
import tifmo.proc.mkSTreeEnglish
import tifmo.stree.InferMgr
import tifmo.stree.Align
import tifmo.stree.PI

import scala.collection.mutable

def confidence(algn: Align, hall: Set[EnWord], cache: mutable.Map[List[Set[String]], (mutable.Map[String, Long], Double)]) = {
	
	val cws = algn.clue.src.init.map(_.term.word.asInstanceOf[EnWord]).toSet
	def trimHead(x: List[PI]) = {
		x.map(_.term.word.asInstanceOf[EnWord]).dropWhile(y => cws.exists(EnWord.judgeSynonym(y, _))).filter(y => !StopWords(y.lex)).toSet
	}
	val tws = if (algn.soft && (algn.tp.src.last.term.word.asInstanceOf[EnWord].ner != "O" || EnWord.judgeSynonym(algn.tp.src.last.term.word.asInstanceOf[EnWord], algn.hp.src.last.term.word.asInstanceOf[EnWord]))) {
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
		
		0.3 + 0.4 / (cws.size + hws.filter(x => cws.exists(EnWord.judgeSynonym(x, _))).size)
		
	} else if (hws.forall(x => tws.exists(EnWord.judgeSynonym(x, _)))) {
		
		0.8
		
	} else if (tws.forall(x => hws.exists(EnWord.judgeSynonym(x, _)))) {
		
		0.1 + 1.0 / (3 + hws.size - tws.size)
		
	} else if (tws.forall(x => hall.exists(EnWord.judgeSynonym(x, _)))) {
		
		0.0
		
	} else if (tws.size <= 3 && hws.size <= 3) {
		
		val cossim = EnWord.cossim(tws, hws, cache)
		1.0 / (1.0 - math.log10(cossim))
		
	} else {
		0.0
	}
}

val output_fea = "output/RTE_dev.fea"
val bw = new BufferedWriter(new FileWriter(output_fea))

val input_xml = "input/RTE_dev.xml"

val f = xml.XML.loadFile(input_xml)
//for (p <- (f \ "pair")) {
val p = <pair id="592" value="TRUE" task="QA">
	<t>He endeared himself to artists by helping them in lean years and following their careers, said Henry Hopkins, chairman of UCLA&apos;s art department, director of the UCLA/Armand Hammer Museum and Cultural Center and former director of the Weisman foundation.</t>
	<h>The UCLA/Hammer Museum is directed by Henry Hopkins.</h>
</pair>
	
	val traw = (p \ "t").text.trim
	val (tstree, twaa) = mkSTreeEnglish(preProcEnglish(traw))
	
	val hraw = (p \ "h").text.trim
	val (hstree, hwaa) = mkSTreeEnglish(preProcEnglish(hraw))
	
	println("text: ")
	println(traw)
	println("---")
	println("hypo: ")
	println(hraw)
	println("----")
	
	val imgr = new InferMgr(hstree)
	imgr.addPremise(tstree)
	
	val twords = tstree.streeNodeList.map(_.word.asInstanceOf[EnWord]).toSet
	val hwords = hstree.streeNodeList.map(_.word.asInstanceOf[EnWord]).toSet
	
	var lap = 0
	for (hw <- hwords) {
		var flag = false
		for (tw <- twords) {
			if (hw.lex == tw.lex) {
				flag = true
			} else if (EnWord.judgeSynonym(hw, tw)) {
				flag = true
				println("Synonym: " + hw + " " + tw)
				imgr.addSynonym(hw, tw)
			} else {
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
	
	val cache = mutable.Map.empty[List[Set[String]], (mutable.Map[String, Long], Double)]
	val (fea2, fea3) = imgr.trace(confidence(_, hwords, cache), 0.2)
	println("--------------")
	
	val fea1 = lap.toDouble / hwords.size
	
	val str = (if ((p \ "@value").text == "TRUE") "+1" else "-1") + " 1:" + fea1 + " 2:" + fea2 + " 3:" + fea3 + "\n"
	bw.write(str)
//}

bw.close()

