import scala.collection.mutable

case class ATreeNode(expr: String, children: Seq[ATreeNode] = Seq.empty)

object ATree {
  def buildTree(exprs: Seq[String]): ATreeNode = {
    val root = ATreeNode("root")
    val queue = mutable.Queue.empty[(ATreeNode, String)]
    queue.enqueue((root, exprs.mkString(" or ")))

    while (queue.nonEmpty) {
      val (parent, expr) = queue.dequeue()
      val input = BooleanExpression(expr)
      val dnf = input.dnf()

      dnf match {
        case BooleanExpression.Or(lhs, rhs) =>
          val leftChild = ATreeNode(lhs.toString)
          val rightChild = ATreeNode(rhs.toString)
          parent.children = Seq(leftChild, rightChild)
          queue.enqueue((leftChild, lhs.toString))
          queue.enqueue((rightChild, rhs.toString))
        case _ =>  // DNF expression has only one term (no OR)
          val child = ATreeNode(dnf.toString)
          parent.children = Seq(child)
      }
    }

    root
  }

  def optimizeCommonSubexprs(root: ATreeNode): ATreeNode = {
    val childrenByExpr = mutable.Map.empty[String, Seq[ATreeNode]]
    val leafNodes = mutable.Buffer.empty[ATreeNode]

    // Traverse the tree and collect all children nodes by their expressions
    def collectChildren(node: ATreeNode): Unit = {
      node.children.foreach {
        case leaf@ATreeNode(_, Seq()) => leafNodes += leaf
        case child =>
          childrenByExpr.getOrElseUpdate(child.expr, Seq.empty) :+= child
          collectChildren(child)
      }
    }
    collectChildren(root)

    // Filter out common subexpressions and replace them with new nodes
    def replaceCommonSubexprs(node: ATreeNode): Option[ATreeNode] = {
      val newChildren = node.children.flatMap { child =>
        replaceCommonSubexprs(child) match {
          case Some(newNode) => Seq(newNode)
          case None => childrenByExpr.get(child.expr).fold(Seq(child)) { sameExprChildren =>
            if (sameExprChildren.length > 1) {
              val newChild = optimizeCommonSubexprs(ATreeNode(child.expr, sameExprChildren))
              childrenByExpr.remove(child.expr)
              childrenByExpr.getOrElseUpdate(newChild.expr, Seq.empty) :+= newChild
              Some(newChild)
            } else {
              Some(child)
            }
          }
        }
      }
      if (newChildren != node.children) Some(node.copy(children = newChildren)) else None
    }

    // Replace common subexpressions iteratively until there are no more changes
    var newRoot = root
    var changed = true
    while (changed) {
      changed = false
      replaceCommonSubexprs(newRoot) match {
        case Some(newNode) =>
          newRoot = newNode
          changed = true
        case None =>
      }
    }

    // Append leaf nodes that weren't used in common subexpression nodes
    if (leafNodes.nonEmpty) {
      val leafNodeExprs = leafNodes.map(_.expr).toSet
      val leafNodesByExpr = leafNodes.groupBy(_.expr)
      val newLeaves = leafNodesByExpr.filterNot { case (expr, _) => childrenByExpr.contains(expr) }.values.flatten.toSeq
      newRoot.copy(children = newRoot.children ++ newLeaves)
    } else {
      newRoot
    }
  }
}