
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.FileOutputStream
import java.io.ObjectOutputStream

import tifmo.proc.addknowEnglish
import tifmo.stree.STree
import tifmo.stree.InferMgr
import tifmo.knowledge.EnWord
import tifmo.knowledge.EnConfiFunc

if (args.length != 1) {
	println("USAGE:\nJAVA_OPTS='-Xmx2g' scala -classpath 'extern' mkTraceData.scala FILENAME")
	sys.exit(1)
}

val input_xml = "input/" + args(0) + ".xml"
val f = xml.XML.loadFile(input_xml)

val input_stree = "output/" + args(0) + ".stree"
val ois = new ObjectInputStream(new FileInputStream(input_stree))

val output_trace = "output/" + args(0) + ".trace"
val oos = new ObjectOutputStream(new FileOutputStream(output_trace))

for (p <- (f \ "pair")) {
	
	val traw = (p \ "t").text.trim
	val hraw = (p \ "h").text.trim
	
	val (tstree, hstree) = ois.readObject().asInstanceOf[(STree, STree)]
	
	println("Pair: id=" + (p \ "@id").text)
	println(traw)
	println(tstree)
	println(hraw)
	println(hstree)
	
	val imgr = new InferMgr(hstree)
	imgr.addPremise(tstree)
	
	val tws = tstree.streeNodeList.map(_.word.asInstanceOf[EnWord]).toSet
	val ws = hstree.streeNodeList.map(_.word.asInstanceOf[EnWord]).toSet ++ tws
	
	addknowEnglish(imgr, ws)
	
	val tr = imgr.trace(new EnConfiFunc(tws), 0.1, 9)
	
	tr.foreach(println(_))
	
	oos.writeObject((tstree, hstree, tr))
	oos.reset()
}
ois.close()
oos.close()

sys.exit(0)
