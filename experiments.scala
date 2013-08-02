
import tifmo.stree.STree
import tifmo.stree.TraceInfo
import tifmo.knowledge.EnWord
import tifmo.proc.featureEnglish
import tifmo.proc.linearClassifier

import java.io.FileInputStream
import java.io.ObjectInputStream

import scala.collection.mutable

def procPair(tstree: STree, hstree: STree, tr: List[TraceInfo]) = {
	
	val tws = tstree.streeNodeList.map(_.word.asInstanceOf[EnWord]).toSet
	val hws = hstree.streeNodeList.map(_.word.asInstanceOf[EnWord]).toSet
	
	val fil = hws.filter(x => tws.exists(y => {
		x.synonymTo(y, true)
	}))
	
	val filnl = hstree.streeNodeList.filter(n => tws.exists(y => {
		val x = n.word.asInstanceOf[EnWord]
		x.synonymTo(y, true)
	}))
	
	val (rpart, ppart) = featureEnglish(tr)
	
	Array(
		if (hws.size == 0) 0.0 else (fil.size.toDouble / hws.size),
		
		//if (tr.head.propnum == 0) 0.0 else (tr.head.propsBefore.toDouble / tr.head.propnum),
		if (tr.head.propnum == 0) 0.0 else (tr.last.propsAfter.toDouble / tr.head.propnum),
		
		if (tr.head.rweight == 0) 0.0 else (tr.head.rpartBefore.toDouble / tr.head.rweight), 
		if (tr.head.rweight == 0) 0.0 else (rpart(0) / tr.head.rweight), 
		if (tr.head.rweight == 0) 0.0 else (rpart(1) / tr.head.rweight), 
		if (tr.head.rweight == 0) 0.0 else (rpart(2) / tr.head.rweight), 
		if (tr.head.rweight == 0) 0.0 else (rpart(3) / tr.head.rweight), 
		//if (tr.head.rweight == 0) 0.0 else (rpart(4) / tr.head.rweight), 
		if (tr.head.pweight == 0) 0.0 else (tr.head.ppartBefore.toDouble / tr.head.pweight), 
		if (tr.head.pweight == 0) 0.0 else (ppart(0) / tr.head.pweight), 
		if (tr.head.pweight == 0) 0.0 else (ppart(1) / tr.head.pweight), 
		if (tr.head.pweight == 0) 0.0 else (ppart(2) / tr.head.pweight), 
		if (tr.head.pweight == 0) 0.0 else (ppart(3) / tr.head.pweight)//, 
		//if (tr.head.pweight == 0) 0.0 else (ppart(4) / tr.head.pweight)
		
	)
}

var devfea = Nil:List[(Boolean, String, Array[Double])]

def readFeas(xmlfn: String, tracefn: String, vl: String, vv: String) = {
	val f = xml.XML.loadFile("input/" + xmlfn)
	val ois = new ObjectInputStream(new FileInputStream("output/" + tracefn))
	
	for (p <- (f \ "pair")) yield {
		val task = {
			val pre = (p \ "@task").text
			if (pre == "RC") "IE" else pre
		}
		val gold = ((p \ vl).text == vv)
		
		val (tstree, hstree, tr) = ois.readObject().asInstanceOf[(STree, STree, List[TraceInfo])]
		
		println(p)
		
		(gold, task, procPair(tstree, hstree, tr))
	}
}

val rte1dev = readFeas("RTE_dev.xml", "RTE_dev.trace", "@value", "TRUE") ++ readFeas("RTE_dev2.xml", "RTE_dev2.trace", "@value", "TRUE")
val rte2dev = readFeas("RTE2_dev.xml", "RTE2_dev.trace", "@entailment", "YES")
val rte3dev = readFeas("RTE3_dev.xml", "RTE3_dev.trace", "@entailment", "YES")
val rte5dev = readFeas("RTE5_dev.xml", "RTE5_dev.trace", "@entailment", "ENTAILMENT")

val rte1test = readFeas("RTE_test.xml", "RTE_test.trace", "@value", "TRUE")
val rte2test = readFeas("RTE2_test.xml", "RTE2_test.trace", "@entailment", "YES")
val rte3test = readFeas("RTE3_test.xml", "RTE3_test.trace", "@entailment", "YES")
val rte5test = readFeas("RTE5_test.xml", "RTE5_test.trace", "@entailment", "ENTAILMENT")

def sepTask(data: Seq[(Boolean, String, Array[Double])]) = {
	val ret = mutable.Map.empty[String, List[(Boolean, String, Array[Double])]]
	for (samp <- data) {
		ret(samp._2) = samp :: ret.getOrElse(samp._2, Nil)
	}
	ret.toMap
}

def modelALL(dev: Seq[(Boolean, String, Array[Double])]) = {
	
	val models = for ((k, samps) <- sepTask(dev)) yield {
		val totrain = for ((gold, task, feas) <- samps) yield {
			assert(task == k)
			val wg = if (gold) 1.0 else -1.0
			wg +: feas
		}
		(k, linearClassifier.train(100, true, totrain))
	}
	
	(samp: (Boolean, String, Array[Double])) => {
		val (gold, task, feas) = samp
		val model = models(task)
		val scr = (model(0) /: (0 until feas.length))((x, i) => x + model(i + 1) * feas(i))
		(scr > 0.0)
	}
}

val modUnSup = (samp: (Boolean, String, Array[Double])) => {
	val (gold, task, feas) = samp
	(feas(1) > 0.5)
}

def accuracy(outs: Seq[Boolean], golds: Seq[(Boolean, String, Array[Double])]) = {
	
	val cnum = (outs zip golds).filter(x => x._1 == x._2._1).length
	cnum.toDouble / outs.length
}

def precision(outs: Seq[Boolean], golds: Seq[(Boolean, String, Array[Double])]) = {
	
	val posi = (outs zip golds).filter(_._1)
	val tp = posi.filter(_._2._1)
	tp.length.toDouble / posi.length
}

def recall(outs: Seq[Boolean], golds: Seq[(Boolean, String, Array[Double])]) = {
	
	val truth = (outs zip golds).filter(_._2._1)
	val tp = truth.filter(_._1)
	tp.length.toDouble / truth.length
}


val rte1Mod = modelALL(rte1dev)
val rte1sepdev = sepTask(rte1dev)
val rte1septest = sepTask(rte1test)
println("RTE1 ")
for (task <- List("IE", "QA", "IR", "MT", "CD", "PP")) {
	val devacc = accuracy(rte1sepdev(task).map(rte1Mod(_)), rte1sepdev(task))
	val testacc = accuracy(rte1septest(task).map(rte1Mod(_)), rte1septest(task))
	println(task + " dev: " + devacc + " test: " + testacc)
}
println("tot dev: " + accuracy(rte1dev.map(rte1Mod(_)), rte1dev) + " test: " + accuracy(rte1test.map(rte1Mod(_)), rte1test))
println("unsupervised dev: " + accuracy(rte1dev.map(modUnSup(_)), rte1dev) + " test: " + accuracy(rte1test.map(modUnSup(_)), rte1test))
println("prec: " + precision(rte1test.map(modUnSup(_)), rte1test) + " rec: " + recall(rte1test.map(modUnSup(_)), rte1test))
println("")

val rte2Mod = modelALL(rte2dev)
val rte2sepdev = sepTask(rte2dev)
val rte2septest = sepTask(rte2test)
println("RTE2 ")
for (task <- List("IE", "QA", "IR", "SUM")) {
	val devacc = accuracy(rte2sepdev(task).map(rte2Mod(_)), rte2sepdev(task))
	val testacc = accuracy(rte2septest(task).map(rte2Mod(_)), rte2septest(task))
	println(task + " dev: " + devacc + " test: " + testacc)
}
//println("IE, dev-prec: " + precision(rte2sepdev("IE").map(rte2Mod(_)), rte2sepdev("IE")) + " dev-recall: " + recall(rte2sepdev("IE").map(rte2Mod(_)), rte2sepdev("IE")))
//println("IE, test-prec: " + precision(rte2septest("IE").map(rte2Mod(_)), rte2septest("IE")) + " test-recall: " + recall(rte2septest("IE").map(rte2Mod(_)), rte2septest("IE")))
println("tot dev: " + accuracy(rte2dev.map(rte2Mod(_)), rte2dev) + " test: " + accuracy(rte2test.map(rte2Mod(_)), rte2test))
println("unsupervised dev: " + accuracy(rte2dev.map(modUnSup(_)), rte2dev) + " test: " + accuracy(rte2test.map(modUnSup(_)), rte2test))
println("prec: " + precision(rte2test.map(modUnSup(_)), rte2test) + " rec: " + recall(rte2test.map(modUnSup(_)), rte2test))
println("")

val rte3Mod = modelALL(rte3dev)
val rte3sepdev = sepTask(rte3dev)
val rte3septest = sepTask(rte3test)
println("RTE3 ")
for (task <- List("IE", "QA", "IR", "SUM")) {
	val devacc = accuracy(rte3sepdev(task).map(rte3Mod(_)), rte3sepdev(task))
	val testacc = accuracy(rte3septest(task).map(rte3Mod(_)), rte3septest(task))
	println(task + " dev: " + devacc + " test: " + testacc)
}
println("IE, dev-prec: " + precision(rte3sepdev("IE").map(rte3Mod(_)), rte3sepdev("IE")) + " dev-recall: " + recall(rte3sepdev("IE").map(rte3Mod(_)), rte3sepdev("IE")))
println("IE, test-prec: " + precision(rte3septest("IE").map(rte3Mod(_)), rte3septest("IE")) + " test-recall: " + recall(rte3septest("IE").map(rte3Mod(_)), rte3septest("IE")))
println("tot dev: " + accuracy(rte3dev.map(rte3Mod(_)), rte3dev) + " test: " + accuracy(rte3test.map(rte3Mod(_)), rte3test))
println("unsupervised dev: " + accuracy(rte3dev.map(modUnSup(_)), rte3dev) + " test: " + accuracy(rte3test.map(modUnSup(_)), rte3test))
println("prec: " + precision(rte3test.map(modUnSup(_)), rte3test) + " rec: " + recall(rte3test.map(modUnSup(_)), rte3test))
println("")

val rte5Mod = modelALL(rte5dev)
val rte5sepdev = sepTask(rte5dev)
val rte5septest = sepTask(rte5test)
println("RTE5 ")
for (task <- List("IE", "QA", "IR")) {
	val devacc = accuracy(rte5sepdev(task).map(rte5Mod(_)), rte5sepdev(task))
	val testacc = accuracy(rte5septest(task).map(rte5Mod(_)), rte5septest(task))
	println(task + " dev: " + devacc + " test: " + testacc)
}
println("tot dev: " + accuracy(rte5dev.map(rte5Mod(_)), rte5dev) + " test: " + accuracy(rte5test.map(rte5Mod(_)), rte5test))
println("unsupervised dev: " + accuracy(rte5dev.map(modUnSup(_)), rte5dev) + " test: " + accuracy(rte5test.map(modUnSup(_)), rte5test))
println("prec: " + precision(rte5test.map(modUnSup(_)), rte5test) + " rec: " + recall(rte5test.map(modUnSup(_)), rte5test))
println("")

