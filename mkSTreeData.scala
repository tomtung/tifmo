
import tifmo.proc.preProcEnglish
import tifmo.proc.mkSTreeEnglish

import java.io.FileOutputStream
import java.io.ObjectOutputStream

if (args.length != 1) {
	println("USAGE:\nJAVA_OPTS='-Xmx2g' scala -classpath 'extern' mkSTreeData.scala FILENAME")
	sys.exit(1)
}

val input_xml = "input/" + args(0) + ".xml"
val f = xml.XML.loadFile(input_xml)

val output_stree = "output/" + args(0) + ".stree"
val oos = new ObjectOutputStream(new FileOutputStream(output_stree))

for (p <- (f \ "pair")) {
	
	val traw = (p \ "t").text.trim
	val tstree = mkSTreeEnglish(preProcEnglish(traw))
	
	val hraw = (p \ "h").text.trim
	val hstree = mkSTreeEnglish(preProcEnglish(hraw))
	
	println("Pair: id=" + (p \ "@id").text)
	println(traw)
	println(tstree)
	println(hraw)
	println(hstree)
	
	oos.writeObject((tstree, hstree))
	oos.reset()
}

oos.close()

sys.exit(0)
