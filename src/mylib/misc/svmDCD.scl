package mylib

import scala.util.Random.shuffle

package misc {
	/**
	 * SVM(L2norm/L1loss) using dual coordinate descent.
	 * 
	 * This function returns the weight vector `w` and `a` to minimize
	 * 
	 * {{{
	 *  (1/2)||w||^2 + \sum_{k}(a_k * {(x_k, w) + xb_k})
	 * }}}
	 * 
	 * such that
	 * 
	 * {{{
	 *  w(i) = -\sum_k{a_k * x_k(i)} (for i = 0, ..., n - 1)
	 *  for (c <- constraints; (cc, c0) <- c) cc.toList.map(z => a(z._1) * z._2).sum <= c0
	 * }}}
	 */
	object svmDCD extends ((Int, Int, Array[Array[Double]], Array[Double], Array[Set[(Map[Int, Double], Double)]], Double, Double) => (Array[Double], Array[Double])) {
		
		/**
		 * @param n Number of features.
		 * @param l Number of data points.
		 * @param x Data points, array of (array of length `n`) of length `l`.
		 * @param xb sign, array of length `l`.
		 * @param constraints Inequalities satisfied by a.
		 * @param tol1 Minimum update of a.
		 * @param tol2 Minimum update of w.
		 * @return Weight vector `w` (array of length `n`) and dual weight vector `a` (array of length `l`).
		 */
		def apply(
			n: Int, 
			l: Int, 
			x: Array[Array[Double]], 
			xb: Array[Double], 
			constraints: Array[Set[(Map[Int, Double], Double)]], 
			tol1: Double = 1e-10, 
			tol2: Double = 1e-7
		) = {
			
			val w = new Array[Double](n)
			val a = new Array[Double](l)
			
			val norm = for (xj <- x) yield {
				xj.map(z => z * z).sum
			}
			
			def dot(u: Array[Double], v: Array[Double]) = (0 until n).map(i => u(i) * v(i)).sum
			
			var flag = false
			
			def update_a(j: Int, d: Double) {
				if (d < -tol1 || d > tol1) {
					a(j) = a(j) + d
					for (i <- 0 until n) {
						val tmp = d * x(j)(i)
						if (tmp < -tol2 || tmp > tol2) flag = true
						w(i) = w(i) - tmp
					}
				}
			}
			
			def calcb(cc: Map[Int, Double], c0: Double, j: Int) = {
				var tmp = c0
				for ((k, v) <- cc; if k != j) {
					tmp -= a(k) * v
				}
				tmp
			}
			
			def checkConstraints(j: Int) = {
				val c = constraints(j)
				var ajMin = Double.MinValue
				var ajMax = Double.MaxValue
				for ((cc, c0) <- c) {
					val ccj = cc(j)
					val tmp = calcb(cc, c0, j) / ccj
					if (ccj < 0.0) {
						if (tmp > ajMin) ajMin = tmp
					} else {
						if (tmp < ajMax) ajMax = tmp
					}
				}
				(ajMin, ajMax)
			}
			
			def descent_a(j: Int) {
				val d = (dot(w, x(j)) + xb(j)) / norm(j)
				val aj = a(j)
				val (ajMin, ajMax) = checkConstraints(j)
				val lb = ajMin - aj
				val ub = ajMax - aj
				if (d <= lb) {
					update_a(j, lb)
				} else if (d >= ub) {
					update_a(j, ub)
				} else {
					update_a(j, d)
				}
			}
			
			def loop() {
				flag = false
				for (j <- shuffle((0 until l).toIndexedSeq)) descent_a(j)
				if (flag) loop()
			}
			loop()
			
			(w, a)
		}
	}
	
}


