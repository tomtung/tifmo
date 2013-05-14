
import tifmo.proc.mkSTreeEnglish

val (stree, waa) = mkSTreeEnglish("British servicemen detained")

println(stree)

waa(0).foreach(x => {
	println(x.surf + " " + x.lex + " " + x.ner)
})
