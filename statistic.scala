
import tifmo.knowledge.EnWord
import tifmo.proc.mkSTreeEnglish


val input_xml = "input/RTE_dev.xml"

val f = xml.XML.loadFile(input_xml)
for (p <- (f \ "pair")) {
	
	val traw = (p \ "t").text.trim
	val tstree = mkSTreeEnglish(traw)
	
	val hraw = (p \ "h").text.trim
	val hstree = mkSTreeEnglish(hraw)
	
	val twords = tstree.streeNodeList.map(_.word).toSet
	val hwords = hstree.streeNodeList.map(_.word).toSet
	
	var lap = 0
	var missne = false
	for (hwpre <- hwords; hw = hwpre.asInstanceOf[EnWord]) {
		twords.find(x => EnWord.judgeSynonym(hw, x.asInstanceOf[EnWord])) match {
			case Some(x) => lap += 1
			case None => {
				if (hw.ner != "O" || hw.lex.matches("[0-9\\.]+")) {
					if (traw.indexOf(hw.surf) == -1) {
						println("missing NE: " + hw.lex)
						missne = true
					}
				}
			}
		}
	}
	if (missne) {
		println("text: ")
		println(traw)
		println("---")
		println("hypo: ")
		println(hraw)
		println("--------------")
	}
}
