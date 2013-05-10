import tifmo.resource.NgramDist
import scala.collection.mutable

val s1 = args(0)
val s2 = args(1)

val ngram = new NgramDist(5, 40)

val ret1 = mutable.Map.empty[String, Long]
val ret2 = mutable.Map.empty[String, Long]

ngram.get(s1, ret1)
ngram.get(s2, ret2)

def norm(x: mutable.Map[String, Long]) = {
	var retsq = 0.0
	for ((k, v) <- x) {
		retsq += v * v
	}
	math.sqrt(retsq)
}

val dot = {
	val ck = ret1.keySet intersect ret2.keySet
	(0.0 /: ck)((a, k) => a + ret1(k) * ret2(k))
}

val cossim = dot / norm(ret1) / norm(ret2)

println(cossim)

