package tifmo

import mylib.res.en.EnTurian10

package main.en {
	
	class EnSimilarityTurian10(val res: EnResources) extends EnSimilarity {
		
		val dim = 50
		
		protected[this] def lookup(x: String) = EnTurian10.lookup(x)
		
		val SynonymFactor = 0.7f
		
	}
	
}
