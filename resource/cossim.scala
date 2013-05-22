import tifmo.resource.EnNgramDist
import scala.collection.mutable
import scala.util.Sorting

val s1 = args(0)
val s2 = args(1)

val ret1 = mutable.Map.empty[String, Long]
val ret2 = mutable.Map.empty[String, Long]

EnNgramDist.get(s1, ret1)
EnNgramDist.get(s2, ret2)

def norm(x: mutable.Map[String, Long]) = {
	if (x.isEmpty) {
		1.0
	} else {
		var retsq = 0.0
		for ((k, v) <- x) {
			retsq += v * v
		}
		math.sqrt(retsq)
	}
}

val dot = {
	val ck = ret1.keySet intersect ret2.keySet
	for (k <- Sorting.stableSort[String, Long](ck.toList, (k:String) => ret1(k) * ret2(k))) {
		println(k)
	}
	(0.0 /: ck)((a, k) => a + ret1(k) * ret2(k))
}

val cossim = dot / norm(ret1) / norm(ret2)

println(cossim)

