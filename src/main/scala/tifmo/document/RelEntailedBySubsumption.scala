package tifmo.document
import tifmo.dcstree.Relation
import tifmo.inference.{ RuleDo, TermIndex, IEngine }

trait RelEntailedBySubsumption extends Relation {
  override def preCheck[T](ex: IEngine, xa: TermIndex, xb: TermIndex) {
    super.preCheck(ex, xa, xb)
    val a = xa.holder
    val b = xb.holder
    ex.ifSubsume(xa, xb, Seq.empty, RuleDo((ie, p, args) => {
      ie.claimRL(a.index, this, b.index)
    }))
  }
}
