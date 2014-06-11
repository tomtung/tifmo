package tifmo

package dcstree {

abstract class WordBase {
  def isStopWord: Boolean

  def isNamedEntity: Boolean

  def isSingleton: Boolean
}

}
