package tifmo

package dcstree {

import tifmo.inference.{ TermIndex, IEngine }

abstract class Relation {

  def execute[T](ex: Executor, a: T, b: T) {
    // Default empty implementation
  }

  def preCheck[T](ex: IEngine, a: TermIndex, b: TermIndex) {
    // Default empty implementation
  }

}

}
