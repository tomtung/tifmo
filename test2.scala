
import tifmo.proc.mkSTreeEnglish
import tifmo.stree.InferMgr
import tifmo.stree.Align

val input_xml = "input/test.xml"

val confidence = (algn: Align) => {
	val bonus = if (algn.soft) 0.5 else 0.0
	bonus + (1.0 / (algn.tp.src.length + algn.hp.src.length))
}

val f = xml.XML.loadFile(input_xml)
for (q <- (f \ "question")) {
	println("======== New Question =========")
	
	val traw = (q \ "text").text
	val (tstree, twaa) = mkSTreeEnglish(traw)
	
	println("text: ")
	println(tstree)
	twaa.foreach(println(_))
	
	for (ans <- (q \ "answer")) {
		val hraw = ans.text
		val (hstree, hwaa) = mkSTreeEnglish(hraw)
		
		println("hypo: ")
		println(hstree)
		hwaa.foreach(println(_))
		println("----")
		
		val imgr = new InferMgr(hstree)
		imgr.addPremise(tstree)
		imgr.trace(confidence, 0.1)
		println("--------------")
	}
}
