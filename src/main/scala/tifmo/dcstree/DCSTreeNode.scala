package tifmo.dcstree

import scala.collection.{immutable, mutable}
import scala.annotation.tailrec

class DCSTreeNode(val children: Set[(DCSTreeEdge, DCSTreeNode)],
                  val rseq: immutable.Seq[SemRole],
                  val token: TokenBase,
                  val sign: Boolean,
                  val selection: Selection,
                  val outRole: SemRole,
                  val context: Set[Context] = Set.empty[Context]) extends Serializable {

  import DCSTreeNode._

  private[this] var aprx: Denotation = null

  def approx = aprx

  private[this] var hfcl: Denotation = null

  def halfcalc = hfcl

  private[this] var otpt: Denotation = null

  def output = otpt

  private[this] val gm = mutable.Map.empty[SemRole, Denotation]

  def germ(r: SemRole) = gm(r)

  private[this] def calcr(denotation: Denotation, role: SemRole): Denotation = {
    children.foreach {
      case (DCSTreeEdgeRelation(`role`, _), childNode) =>
        childNode.upward()
        childNode.downward(null)
      case _ =>
    }

    val s: Set[Denotation] = children.collect {
      case (DCSTreeEdgeQuantifier(`role`, quantifier), node) =>
        node.upward()
        node.downward(null)
        DenotationDI(quantifier, denotation, node.output, role)
    }
    if (s.isEmpty) {
      DenotationPI(denotation, denotation.roles - role)
    } else if (s.size == 1) {
      s.head
    } else {
      DenotationIN(s)
    }
  }

  def upward() {
    if (otpt == null) {
      val rs = rseq.toSet
      val c = if (token.getWord.isStopWord) DenotationW(rs) else DenotationWordSign(rs, token.getWord, sign)
      val s1 = for ((DCSTreeEdgeNormal(r), n) <- children) yield {
        n.upward()
        (r, n.output)
      }
      val s2 = for (ContextA(r, ref) <- context) yield {
        (r, ref.getDenotation)
      }
      val pre = makeINCPW(c, s1 ++ s2)
      aprx = if (selection == null) pre else DenotationSelection(selection, pre)
      hfcl = (approx /: rseq.takeWhile(_ != outRole))((d, r) => {
        gm(r) = makePI(d, r)
        for ((edge@DCSTreeEdgeNormal(rr), n) <- children; if rr == r) {
          n.downward((this, edge))
        }
        calcr(d, r)
      })
      otpt = makePI(halfcalc, outRole)
    }
  }

  private[this] var prt: (DCSTreeNode, DCSTreeEdgeNormal) = null

  def parent = prt

  private[this] var downFlag = false
  private[this] var posinega = true

  def positiveTree = {
    assert(downFlag)
    posinega
  }

  def downward(p: (DCSTreeNode, DCSTreeEdgeNormal)) {
    if (downFlag) {
      assert(parent == p)
      assert(posinega)
    } else {
      assert(p == null || p._1.children.contains((p._2, this)))
      prt = p
      downFlag = true
      val tmp1 = if (p == null) Set(output) else Set(p._1.germ(p._2.inRole))
      val tmp2 = for (ContextB(ref) <- context) yield {
        ref.getDenotation
      }
      val tmp = tmp1 ++ tmp2
      gm(outRole) = if (tmp.size == 1) {
        tmp.head
      } else {
        DenotationIN(tmp.map(DenotationRelabel(_, outRole)))
      }
      for ((edge@DCSTreeEdgeNormal(rr), n) <- children; if rr == outRole) {
        n.downward((this, edge))
      }
      val halfdash = if (p == null && tmp2.isEmpty) {
        halfcalc
      } else {
        makeINCPW(halfcalc, Set((outRole, germ(outRole))))
      }
      (calcr(halfdash, outRole) /: rseq.dropWhile(_ != outRole).tail)((d, r) => {
        gm(r) = makePI(d, r)
        for ((edge@DCSTreeEdgeNormal(rr), n) <- children; if rr == r) {
          n.downward((this, edge))
        }
        calcr(d, r)
      })
    }
  }

  def downwardNega(p: (DCSTreeNode, DCSTreeEdgeNormal)) {
    if (downFlag) {
      assert(parent == p)
      assert(!posinega)
    } else {
      assert(p == null || p._1.children.contains((p._2, this)))
      prt = p
      posinega = false
      downFlag = true
      gm(outRole) = if (p == null) output else p._1.germ(p._2.inRole)
      for ((edge@DCSTreeEdgeNormal(rr), n) <- children; if rr == outRole) {
        n.downwardNega((this, edge))
      }
      val halfdash = if (p == null) {
        halfcalc
      } else {
        makeINCPW(halfcalc, Set((outRole, germ(outRole))))
      }
      calcr(halfdash, outRole)
      (DenotationPI(halfdash, halfdash.roles - outRole) /: rseq.dropWhile(_ != outRole).tail)((d, r) => {
        gm(r) = makePI(d, r)
        for ((edge@DCSTreeEdgeNormal(rr), n) <- children; if rr == r) {
          n.downwardNega((this, edge))
        }
        calcr(d, r)
        DenotationPI(d, d.roles - r)
      })
    }
  }

  val outIndex = rseq.indexOf(outRole)

  def compare2outRole(thisRole: SemRole) = {
    rseq.indexOf(thisRole) - outIndex
  }

  @transient lazy val ascending: Seq[(DCSTreeNode, DCSTreeEdgeNormal)] = {
    if (parent == null) {
      Seq.empty[(DCSTreeNode, DCSTreeEdgeNormal)]
    } else {
      if (parent._1.compare2outRole(parent._2.inRole) < 0) {
        Seq(parent)
      } else {
        parent +: parent._1.ascending
      }
    }
  }

  @transient lazy val descending: Set[(Seq[(DCSTreeEdgeNormal, DCSTreeNode)], Ref)] = {
    if (selection == null) {
      children.flatMap {
        case (e: DCSTreeEdgeNormal, n) => {
          n.descending.map(x => ((e, n) +: x._1, x._2)) + ((Seq((e, n)), RefOutput(n): Ref))
        }
        case _ => Set.empty[(Seq[(DCSTreeEdgeNormal, DCSTreeNode)], Ref)]
      }
    } else {
      Set.empty[(Seq[(DCSTreeEdgeNormal, DCSTreeNode)], Ref)]
    }
  }

  def copy: DCSTreeNode = new DCSTreeNode(
    children.map(x => (x._1, x._2.copy)),
    rseq,
    token,
    sign,
    selection,
    outRole,
    context
  )

  @transient override lazy val toString: String = {
    val builder = new StringBuilder()

    // Add first line for root node to builder

    val rootLinePrefix = {
      val signStr = if (sign) "" else ", ¬"

      val selectionStr =
        if (selection == null) ""
        else ", " + selection.toString

      if (signStr.isEmpty && selectionStr.isEmpty) {
        outRole.toString
      } else {
        "[%s%s%s]".format(outRole.toString, signStr, selectionStr)
      }
    } + " ───> "

    builder.append(rootLinePrefix).append(token.getWord).append({
      if (!rseq.isEmpty) rseq.mkString(" (", ", ", ")")
      else ""
    })

    // Add subsequent lines for children nodes to builder, with proper indentation

    val lineStartRegex = "(?m)^"

    val childrenStrings: List[String] =
      for (
        role <- rseq.toList.distinct;
        (childEdge, childNode) <- children.withFilter(_._1.inRole == role)
      ) yield {
        val childLinePrefix = "─── " + (childEdge match {
          case DCSTreeEdgeNormal(inRole) =>
            inRole.toString
          case DCSTreeEdgeQuantifier(inRole, QuantifierALL) =>
            "[%s, ALL]".format(inRole.toString)
          case DCSTreeEdgeQuantifier(inRole, QuantifierNO) =>
            "[%s, NO]".format(inRole.toString)
          case DCSTreeEdgeQuantifier(inRole, qualifier) =>
            "[%s, %s]".format(inRole.toString, qualifier.toString)
          case DCSTreeEdgeRelation(inRole, relation) =>
            "[%s, %s]".format(inRole.toString, relation.toString)
          case edge =>
            "[%s]".format(edge.toString)
        }) + " / "

        (childLinePrefix + childNode).
          replaceAll(lineStartRegex, " " * childLinePrefix.length).substring(childLinePrefix.length)
      }

    @tailrec
    def addChildrenStrings(strList: List[String]) {
      strList match {
        case childStr :: Nil =>
          builder.
            append(sys.props("line.separator")).
            append(
              ("└" + childStr).
                replaceAll(lineStartRegex, " " * (rootLinePrefix.length + 1)).
                substring(1)
            )
        case childStr :: tail =>
          builder.
            append(sys.props("line.separator")).
            append(
              childStr.
                replaceAll(lineStartRegex, "│").
                replaceAll(lineStartRegex, " " * rootLinePrefix.length)
            )
          addChildrenStrings(tail)
        case Nil => // Do nothing
      }
    }

    addChildrenStrings(childrenStrings)

    builder.toString()
  }
}

private object DCSTreeNode {

  private def makePI(x: Denotation, r: SemRole): Denotation = {
    if (x.roles.size == 1) {
      assert(x.roles.head == r)
      x
    } else {
      DenotationPI(x, Set(r))
    }
  }

  private def makeINCPW(c: Denotation, s: Set[(SemRole, Denotation)]): Denotation = {
    if (s.isEmpty) {
      c
    } else {
      val rs = s.map(_._1)
      assert(rs.subsetOf(c.roles))
      val side = for (r <- rs) yield {
        val fil = s.filter(_._1 == r).map(x => DenotationRelabel(x._2, x._1): Denotation)
        if (fil.size <= 1) fil.head else DenotationIN(fil)
      }
      val rest = c.roles -- rs
      val precp = if (rest.isEmpty) side else side + DenotationW(rest)
      val cp = if (precp.size == 1) precp.head else DenotationCP(precp)
      DenotationIN(Set(c, cp))
    }
  }
}
