package tifmo

import mylib.res.en.EnMikolov13

package main.en {
	
	class EnSimilarityMikolov13(val res: EnResources) extends EnSimilarity {
		
		val dim = 300
		
		protected[this] def lookup(x: String) = EnMikolov13.lookup(x)
		
		val SynonymFactor = 0.7f
		
	}
	
}
