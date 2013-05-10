
import tifmo.proc.mkSTreeEnglish
import tifmo.stree.InferMgr
import tifmo.stree.Align

//val traw = "Jack Straw, the Foreign Secretary, will meet his Brazilian counterpart, Celso Amorim, in London today."
//val hraw = "Jack Straw is a partner of Celso Amorim."

//val traw = "Angola as a Portuguese colony achieved independence in 1975."
//val hraw = "Angola became independent from Spain in the 1970s."

val traw = "Anyway, maybe it's best not to plan everything."
val hraw = "Don't plan everything."

val tstree = mkSTreeEnglish(traw)
val hstree = mkSTreeEnglish(hraw)

println("============ Test Start =============")
println(tstree)
println("---")
println(hstree)
println("----")

val imgr = new InferMgr(hstree)
imgr.addPremise(tstree)

//imgr.addSynonym(tstree.streeNodeList(3).word, hstree.streeNodeList(3).word) // independence = independent
//imgr.addHypernym(tstree.streeNodeList(4).word, hstree.streeNodeList(2).word) // 1975 -> 1970s

val confidence = (algn: Align) => {
	val bonus = if (algn.soft) 0.5 else 0.0
	bonus + (1.0 / (algn.tp.src.length + algn.hp.src.length))
}

imgr.trace(confidence, 0.1)
