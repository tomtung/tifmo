
import tifmo.proc.preProcEnglish
import tifmo.proc.mkSTreeEnglish
import tifmo.proc.addknowEnglish
import tifmo.stree.InferMgr
import tifmo.knowledge.EnWord
import tifmo.knowledge.EnConfiFunc

//val traw = """"Beatrice and Benedict" is an overture by Berlioz."""
//val hraw = """The program will include Falla's "Night in the Gardens of Spain," Ravel's Piano Concerto in G, Berlioz's Overture to "Beatrice and Benedict," and Roy Harris' Symphony No. 3."""

//val traw = "To the world, M. Larry Lawrence, the new U.S. emissary to Switzerland who hosted President Clinton on his Southern California vacation, will be known as Mr. Ambassador."
//val hraw = "Larry Lawrence is the head of the U.S. Embassy in Switzerland."

val traw = "Jack Straw, the Foreign Secretary, will meet his Brazilian counterpart, Celso Amorim, in London today."
val hraw = "Jack Straw is a partner of Celso Amorim."

//val traw = "The watchdog International Atomic Energy Agency meets in Vienna on September 19."
//val hraw = "The International Atomic Energy Agency holds a meeting in Vienna."

//val traw = "The watchdog IAEA meets in Vienna on September 19."
//val hraw = "The IAEA holds a meeting in Vienna."

//val traw = "Angola as a Portuguese colony achieved independence in 1975."
//val hraw = "Angola became independent from Spain in the 1970s."

//val traw = "Angola as a Portuguese colony achieved independence in 1975."
//val hraw = "Angola became independent from Portugal in the 1970s."

//val traw = "Anyway, maybe it's best not to plan everything."
//val hraw = "Don't plan everything."

val tstree = mkSTreeEnglish(preProcEnglish(traw))
val hstree = mkSTreeEnglish(preProcEnglish(hraw))

println("============ Test Start =============")
println(traw)
println(tstree)
println(hraw)
println(hstree)

val imgr = new InferMgr(hstree)
imgr.addPremise(tstree)

val ws = hstree.streeNodeList.map(_.word.asInstanceOf[EnWord]).toSet ++ tstree.streeNodeList.map(_.word.asInstanceOf[EnWord])
addknowEnglish(imgr, ws)

val tr = imgr.trace(new EnConfiFunc, 0.1)

tr.foreach(println(_))

sys.exit(0)
