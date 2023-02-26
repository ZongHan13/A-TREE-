import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object testApp2 extends App{

trait  Node {
    var expression: String
    var parent: ListBuffer[Node] 
    var childs : ListBuffer[Node] 
    var useCount: Int
}

class inner_Node(_expression: String) extends Node {
  var expression: String = _expression
  var parent: ListBuffer[Node] = ListBuffer[Node]()
  var childs: ListBuffer[Node] = ListBuffer[Node]()
  var useCount = 0
}

class leaf_Node(_expression: String) extends Node {
  var expression: String = _expression
  var parent: ListBuffer[Node] = ListBuffer[Node]()
  var childs: ListBuffer[Node] = ListBuffer[Node]()
  var useCount = 0
}

class ATree() {

    var hen: HashMap[Int, Node] = HashMap[Int, Node]()
    var root: ListBuffer[Node] = ListBuffer[Node]()

    def Insert(_expression: String): Node = {
        val id = generateID(_expression)
        if(hen.getOrElse(id,0) != 0) {
            hen(id).useCount += 1
            //hen(id)
        } else {
            val childExprs = _expression.replace("^","").replace("v","").toList
            if (_expression.length() > 1) {
                for(childExpr <- childExprs) {
                val childNode = Insert(childExpr.toString())
                hen(id).childs += childNode
                }
            }
            
            if (_expression.length() > 1) {
                val new_node = new inner_Node(_expression)
                hen += (id -> new_node)
                
            } else {
                val new_node = new leaf_Node(_expression)
                hen += (id -> new_node)
            }
        }
       
        hen(id)
    }

    private def generateID(_expression: String): Int = {
            val predicatesAndOperators: List[Char] = _expression.toList 
            var id: Int = 0
            for (char <- predicatesAndOperators) {
                id += char.hashCode()
            }
            id
        }

}

val tree = new ATree()
tree.Insert("A^B")


}
