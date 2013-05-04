
import tifmo.proc.mkSTreeEnglish
import tifmo.stree.InferMgr
import tifmo.stree.Align

val traw = "Jack Straw, the Foreign Secretary, will meet his Brazilian counterpart, Celso Amorim, in London today."
val hraw = "Jack Straw is a partner of Celso Amorim."

val (tstree, twaa) = mkSTreeEnglish(traw)
val (hstree, hwaa) = mkSTreeEnglish(hraw)

println("============ Test Start =============")
println(tstree)
twaa.foreach(println(_))
println("---")
println(hstree)
hwaa.foreach(println(_))
println("----")

val imgr = new InferMgr(hstree)
imgr.addPremise(tstree)

val confidence = (algn: Align) => {
	val bonus = if (algn.soft) 0.5 else 0.0
	bonus + (1.0 / (algn.tp.src.length + algn.hp.src.length))
}

imgr.trace(confidence, 0.1)
