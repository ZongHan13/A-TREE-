sealed trait ANode

case class InnerNode(op: String, left: ANode, right: ANode) extends ANode {
  override def toString: String = s"($left $op $right)"
}

case class LeafNode(value: String) extends ANode {
  override def toString: String = value
}

object ATree {

  def buildDNF(clauses: Seq[Seq[String]]): ANode = {
    val andNodes = clauses.map(buildCNF)
    andNodes.reduce(InnerNode("∧", _, _))
  }

  def buildCNF(clause: Seq[String]): ANode = {
    val orNodes = clause.map(InnerNode)
    orNodes.reduce(InnerNode("∨", _, _))
  }

  def buildTree(expression: String): ANode = {
    val tokens = expression.split("\\^").map(_.trim)
    build(tokens)
  }

  def build(tokens: Array[String]): ANode = {
    var currentNode: ANode = LeafNode(tokens.head)
    for (i <- 1 until tokens.length) {
      if (tokens(i) == "^") {
        currentNode = InnerNode(tokens(i), currentNode, LeafNode(tokens(i + 1)))
      } else {
        val subtree = build(tokens.drop(i))
        currentNode match {
          case InnerNode(`tokens(i)`, left, right) =>
            currentNode = InnerNode(tokens(i), left, InnerNode("^", right, subtree))
            return currentNode
          case _ =>
            currentNode = InnerNode(tokens(i), currentNode, subtree)
        }
      }
    }
    currentNode
  }

  def optimize(root: ANode): ANode = {
    def optimizeHelper(node: ANode, context: Map[String, ANode]): (ANode, Map[String, ANode]) = node match {
      case innerNode @ InnerNode(op, left, right) =>
        val (newLeft, leftContext) = optimizeHelper(left, context)
        val (newRight, rightContext) = optimizeHelper(right, leftContext)
        val newContext = rightContext
        if (op == "^") {
          (InnerNode("^", newLeft, newRight), newContext)
        } else {
          val optimizedLeft = leftContext.get(newLeft.toString) match {
            case Some(existingNode) =>
              existingNode
            case None =>
              newLeft
          }
          val optimizedRight = newContext.get(newRight.toString) match {
            case Some(existingNode) =>
              existingNode
            case None =>
              newRight
          }
          val newNode = InnerNode("v", optimizedLeft, optimizedRight)
          (newNode, newContext + (newNode.toString -> newNode))
        }
      case leafNode @ LeafNode(value) =>
        (context.getOrElse(leafNode.toString, leafNode), context)
    }

    optimizeHelper(root, Map.empty)._1
  }
}

val expr1 = "A^D^B^E"
val expr2 = "A^B^E"
val expr3 = "B^E^D"

val tree1 = ATree.buildTree(expr1)
val tree2 = ATree.buildTree(expr2)
val tree3 = ATree.buildTree(expr3)

val optimizedTree1 = ATree.optimize(tree1)
val optimizedTree2 = ATree.optimize(tree2)
val optimizedTree3 = ATree.optimize(tree3)