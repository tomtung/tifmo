package tifmo.demo

import tifmo.inference.{ RuleDo, IEngine, IEPredSubsume, IEPredRL }
import tifmo.main.en.{ ARG, EnWord, parse }
import mylib.res.en.EnWordNet
import tifmo.document.{ Document, RelPartialOrder }
import tifmo.dcstree._

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
        System.out.flush();
        if (fracas_answer != answer) {
          System.err.println(id + " T: " + t)
          System.err.println(id + " H: " + h)
          System.err.flush();
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
    // For each statements "A[ARG] ⊂ B[ARG]" in the premise,
    // if both A and B are "coherent" (as recursively defined by isCoherent() function)
    // regarding the common subset of their role sets, we also claim "A ⊂ B"

    def isCoherent(denotation: Denotation, roles: Set[SemRole]): Boolean = {
      denotation match {
        case DenotationWordSign(wordRoles, _, true) => roles.forall(wordRoles)

        case DenotationIN(intersected) => intersected.exists(isCoherent(_, roles))

        case DenotationCP(factors) => factors.exists(isCoherent(_, roles))

        case DenotationPI(projected, projectedRoles) => roles.forall(projectedRoles) && isCoherent(projected, roles)

        case _ => false
      }
    }

    def projectedIndex(denotation: Denotation, roles: Set[SemRole]) = {
      val t = ie.getTerm(denotation)
      if (denotation.roles == roles) {
        t
      } else {
        ie.getPI(t, roles)
      }
    }.index

    val argSingletonSet: Set[SemRole] = Set(ARG)
    for (
      StatementSubsume(DenotationPI(sub, `argSingletonSet`), DenotationPI(sup, `argSingletonSet`)) <- premStatements;
      commonRoles = sub.roles intersect sup.roles if isCoherent(sub, commonRoles) && isCoherent(sup, commonRoles)
    ) {
      ie.claimSubsume(
        projectedIndex(sub, commonRoles),
        projectedIndex(sup, commonRoles)
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
