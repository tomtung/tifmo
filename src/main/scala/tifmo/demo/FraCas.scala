package tifmo.demo

import tifmo.inference.{ IEPredRL, IEPredSubsume, RuleDo, IEngine }
import tifmo.main.en.{ ARG, parse, EnWord }
import mylib.res.en.EnWordNet
import tifmo.document.{ Document, RelPartialOrder }
import tifmo.dcstree.{ SemRole, DenotationWordSign, DenotationPI, StatementSubsume }
import scala.collection.immutable.Set

object FraCas {
  def main(args: Array[String]) {
    if (args.length != 1) {
      println("USAGE: tifmo.demo.FraCaS fracas_xml")
      sys.exit()
    }

    val fracas_xml = args(0)
    val f = xml.XML.loadFile(fracas_xml)

    for (p <- f \ "problem") {

      val id = (p \ "@id").text

      val sm =
        if ((p \ "p").length == 1) "single"
        else "multi"

      val fracas_answer = (p \ "@fracas_answer").text

      if (fracas_answer == "undef") {
        println("%s,%s,undef,ignore".format(id, sm))
      } else {
        val t = (p \ "p").map(_.text.trim).mkString(" ")
        val h = (p \ "h").text.trim

        val (tdoc, hdoc) = parse(t, h)

        val answer =
          if (tryProve(tdoc, hdoc)) {
            "yes"
          } else {
            // negate hdoc
            for (n <- hdoc.allRootNodes) {
              n.rootNeg = !n.rootNeg
            }

            if (tryProve(tdoc, hdoc)) {
              "no"
            } else {
              "unknown"
            }
          }

        println("%s,%s,%s,%s".format(id, sm, fracas_answer, answer))
        if (fracas_answer != answer) {
          System.err.println(id + " T: " + t)
          System.err.println(id + " H: " + h)
        }
      }
    }
  }

  def tryProve(tdoc: Document, hdoc: Document): Boolean = {
    val prem = tdoc.makeDeclaratives
    val hypo = hdoc.makeDeclaratives

    val premStatements = prem.flatMap(_.toStatements)
    val hypoStatements = hypo.flatMap(_.toStatements)

    val ie = new IEngine()

    premStatements.foreach(ie.claimStatement)
    hypoStatements.foreach(ie.checkStatement)

    // TODO Refactor so that common semantic knowledge is re-usable

    // Adding the following heuristic:
    // For each statements "A[ARG] ⊂ B[ARG]" in the premise, if both A and B are denotation for words,
    // we also claim "A ⊂ B"
    val argSingletonSet: Set[SemRole] = Set(ARG)
    for (
      StatementSubsume(
        DenotationPI(subWord @ DenotationWordSign(_, _, true), `argSingletonSet`),
        DenotationPI(supWord @ DenotationWordSign(_, _, true), `argSingletonSet`)
        ) <- premStatements
    ) {
      val commonRoles = subWord.roles intersect supWord.roles
      def projectedIndex(d: DenotationWordSign) = {
        val t = ie.getTerm(d)
        if (d.roles == commonRoles) {
          t
        } else {
          ie.getPI(t, commonRoles)
        }
      }.index

      ie.claimSubsume(
        projectedIndex(subWord),
        projectedIndex(supWord)
      )
    }

    // If two words are have equivalent stems according to WordNet, claim their equivalency
    val words = tdoc.allContentWords[EnWord] ++ hdoc.allContentWords[EnWord]
    for (a :: b :: Nil <- words.subsets(2).map(_.toList)) {
      if (EnWordNet.stem(a.lemma, a.mypos) == EnWordNet.stem(b.lemma, b.mypos)) {
        ie.subsume(a, b)
        ie.subsume(b, a)
      }
    }

    // Adding semantic knowledge for handling fracas-220
    for (
      r @ RelPartialOrder(lemma) <- tdoc.allRelations;
      deno <- ie.allDenotationWordSign.find(_.word.asInstanceOf[EnWord].lemma == lemma)
    ) {
      val term = ie.getTerm(deno)
      ie.foreachSubset(term.index, Seq.empty, RuleDo((ie, pred, arg) => {
        pred match {
          case IEPredSubsume(sub, _) =>
            ie.foreachARLX(sub, Seq.empty, RuleDo((ie, rlPred, arg) => {
              rlPred match {
                case IEPredRL(`sub`, `r`, b) =>
                  ie.claimSubsume(b, term.index)
              }
            }))
        }
      }))
    }

    ie.explore()

    hypoStatements.nonEmpty && hypoStatements.forall(ie.checkStatement)
  }
}
