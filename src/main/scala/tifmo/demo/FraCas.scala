package tifmo.demo

import tifmo.inference.IEngine
import tifmo.main.en.parse

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
      val fracas_answer = (p \ "@fracas_answer").text
      if (fracas_answer == "undef") {
        println(id + "," + "undef,ignore")
      } else {
        val t = (p \ "p").map(_.text.trim).mkString("", " ", "")
        val h = (p \ "h").text.trim

        val (tdoc, hdoc) = parse(t, h)

        val prem = tdoc.makeDeclaratives.flatMap(_.toStatements)
        val hypo = hdoc.makeDeclaratives.flatMap(_.toStatements)

        val ie = new IEngine

        prem.foreach(ie.claimStatement)
        hypo.foreach(ie.checkStatement)

        if (hypo.forall(ie.checkStatement)) {
          println(id + "," + fracas_answer + ",yes")
        } else {
          // negate hdoc
          for (n <- hdoc.allRootNodes) {
            n.rootNeg = !n.rootNeg
          }

          val nhypo = hdoc.makeDeclaratives.flatMap(_.toStatements)
          val nie = new IEngine

          prem.foreach(nie.claimStatement)
          nhypo.foreach(nie.checkStatement)

          if (nhypo.forall(nie.checkStatement)) {
            println(id + "," + fracas_answer + ",no")
          } else {
            println(id + "," + fracas_answer + ",unknown")
          }
        }
      }
    }
  }
}
