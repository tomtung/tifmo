package tifmo


package main.en {
	/**
	 * Cosine similarities of word vectors.
	 */
	abstract class EnSimilarity {
		
		val dim: Int
		
		protected[this] def lookup(x: String): Array[Float] 
		
		val res: EnResources
		
		val SynonymFactor: Float
		
		///////////////////////
		
		protected[this] def arrayNorm(x: Array[Float]) = {
			val tmp = x.map(z => z * z).sum
			if (tmp == 0.0) 0.0 else math.sqrt(tmp)
		}
		protected[this] def arrayDot(x: Array[Float], y: Array[Float]) = {
			(for (i <- 0 until dim) yield (x(i) * y(i))).sum
		}
		protected[this] def arraySum(x: Array[Float], y: Array[Float]) = {
			val ret = new Array[Float](dim)
			for (i <- 0 until dim) ret(i) = x(i) + y(i)
			ret
		}
		
		def similarity(as: Iterable[EnWord], bs: Iterable[EnWord]) = {
			
			val avecs = as.toList.map(x => {
				val tmp = ((new Array[Float](dim)) /: x.lemma.split(" ").map(lookup(_)))(arraySum(_, _))
				if (!bs.exists(y => res.semrel(x, y) || res.hyponym(x, y))) {
					tmp.map(_ * SynonymFactor)
				} else if (x.mypos == "D") {
					new Array[Float](dim)
				} else {
					tmp
				}
			})
			
			val bvecs = bs.toList.map(x => {
				val tmp = ((new Array[Float](dim)) /: x.lemma.split(" ").map(lookup(_)))(arraySum(_, _))
				if (!as.exists(y => res.semrel(y, x) || res.hyponym(y, x))) {
					tmp.map(_ * SynonymFactor)
				} else if (x.mypos == "D") {
					new Array[Float](dim)
				} else {
					tmp
				}
			})
			
			val atot = ((new Array[Float](dim)) /: avecs)(arraySum(_, _))
			val btot = ((new Array[Float](dim)) /: bvecs)(arraySum(_, _))
			(arrayDot(atot, btot) / (arrayNorm(atot) * arrayNorm(btot))).max(0.0)
		}
		
		def similarity(a: EnWord, b: EnWord): Double = similarity(List(a), List(b))
		
		def similarity(as: Iterable[EnWord], b: EnWord): Double = similarity(as, List(b))
		
	}
	
}
