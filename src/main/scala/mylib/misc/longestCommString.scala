package mylib


package misc {
	
	object longestCommString {
		
		def length[T](a: Seq[T], b: Seq[T]) = {
			val tab = Array.ofDim[Int](a.length + 1, b.length + 1)
			var max = 0
			for (i <- 1 to a.length; j <- 1 to b.length) {
				if (a(i - 1) == b(j - 1)) {
					val tmp = tab(i - 1)(j - 1) + 1
					tab(i)(j) = tmp
					if (tmp > max) max = tmp
				}
			}
			max
		}
		
		def rateAve[T](a: Seq[T], b: Seq[T]) = {
			2.0 * length[T](a, b) / (a.length + b.length)
		}
		
		def rateMin[T](a: Seq[T], b: Seq[T]) = {
			length[T](a, b) / (a.length min b.length)
		}
		
		def rateMax[T](a: Seq[T], b: Seq[T]) = {
			length[T](a, b) / (a.length max b.length)
		}
		
	}
	
}

