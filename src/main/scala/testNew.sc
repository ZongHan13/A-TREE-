import scala.util.control.Breaks._

case class BooleanExpression(expression:String) {
    def isSimilarTo(other: BooleanExpression): Boolean = {
        expression == other.expression
    }
}
case class Value(value:Boolean)


class ATreeNode(var parent: ATreeNode, var expression: BooleanExpression) {
  var children: List[ATreeNode] = List()
  var values: Set[Value] = Set()

  def addValue(value: Value): Unit = {
    values += value
  }

  def removeValue(value: Value): Unit = {
    values -= value
  }
}

class ATree {
  var root: ATreeNode = _

  def insert(expression: BooleanExpression, value: Value): Unit = {
    if (root == null) {
      root = new ATreeNode(null, expression)
      root.addValue(value)
    } else {
      insert(root, expression, value)
    }
  }

  private def insert(node: ATreeNode, expression: BooleanExpression, value: Value): Unit = {
    if (expression.isSimilarTo(node.expression)) {
      node.addValue(value)
    } else {
      var child: ATreeNode = null
      for (c <- node.children) {
        if (expression.isSimilarTo(c.expression)) {
          child = c
          break
        }
      }
      if (child == null) {
        child = new ATreeNode(node, expression)
        node.children ::= child
      }
      child.addValue(value)
    }
  }

  def search(expression: BooleanExpression): Set[Value] = {
    if (root == null) {
      Set()
    } else {
      search(root, expression)
    }
  }

  private def search(node: ATreeNode, expression: BooleanExpression): Set[Value] = {
    if (expression.isSimilarTo(node.expression)) {
      node.values
    } else {
      var result: Set[Value] = Set()
      for (c <- node.children) {
        if (expression.isSimilarTo(c.expression)) {
          result = result ++ search(c, expression)
        }
      }
      result
    }
  }

  def delete(expression: BooleanExpression, value: Value): Unit = {
    if (root == null) {
      return
    }
    delete(root, expression, value)
    if (root.children.isEmpty && root.values.isEmpty) {
      root = null
    }
  }

  private def delete(node: ATreeNode, expression: BooleanExpression, value: Value): Unit = {
    if (expression.isSimilarTo(node.expression)) {
      node.removeValue(value)
    } else {
        
      for (c <- node.children) {
        if (expression.isSimilarTo(c.expression)) {
          delete(c, expression, value)
          if (c.children.isEmpty && c.values.isEmpty) {
            node.children = node.children.filter(_ != c)
          }
          break
        }
      }
        
    }
  }
}

val tree = new ATree
val expression1 = new BooleanExpression("A^B^C")
val expression2 = new BooleanExpression("B^C^D")
val value1 = new Value(false)
val value2 = new Value(false)

tree.insert(expression1, value1)
tree.insert(expression2, value2)
println(tree.root.children(0).children) // 1
val result1 = tree.search(expression1)
println(result1) // Set(value1)

val result2 = tree.search(expression2)
println(result2) // Set(value2)

tree.delete(expression1, value1)

val result3 = tree.search(expression1)
println(result3) // Set()

val result4 = tree.search(expression2)
println(result4) // Set(value2)