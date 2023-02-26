import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._

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
  var childExprs: List[String] = List[String]()
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

    def insert(_expression: String): Node = {
        val id = generateID(_expression)
        if(hen.getOrElse(id,0) != 0) {
            hen(id).useCount += 1
            //hen(id)
        } else {

            if (_expression.length() > 1) {
                val new_node = new inner_Node(_expression)
                hen += (id -> new_node)
                root += hen(id)
            val childExprs = _expression.replace('^',' ').replace('∨',' ').split(' ').toList
            if (_expression.length() > 1) {
                for(childExpr <- childExprs) {
                val childNode = insert(childExpr.toString())
                hen(id).childs += childNode
                }
            }
            
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


    private def reorganize(_expression: String):Unit = {
        var u = _expression.replace("^","").toSet
        var c : Set[String] = Set.empty

        while (u.nonEmpty) {
            val s = find_max_intersect(charSetToStringSet(u), hen)
            if( s.isEmpty) {
                break()
            }
            u = u.diff(stringSetToCharSet(s))
            c = c.union(s)
        }
        
    }

    private def find_max_intersect(set1:Set[String], target: HashMap[Int, Node]): Set[String] = {
        var maxinterSet: Set[String] = Set.empty
        for((id, node) <- target) {
            val interSet = set1.intersect(exprToStringSet(node.expression))
            if (interSet.size > maxinterSet.size) {
                maxinterSet = interSet
            }
        }

        maxinterSet
        
    }
    
    def charSetToStringSet(set: Set[Char]): Set[String] = {
        var stringSet: Set[String] = Set.empty
        for (char <- set) {
            stringSet += char.toString()
        }
        stringSet
    }

    def stringSetToCharSet(set: Set[String]): Set[Char] = {
        var charSet: Set[Char] = Set.empty
        for (string <- set) {
            charSet += string.charAt(0)
        }
        charSet
    }

    def exprToStringSet(expr: String): Set[String] = {
        var stringSet: Set[String] = Set.empty
        for (char <- expr) {
            stringSet += char.toString()
        }
        stringSet
    }
    

}

val tree = new ATree()
tree.insert("A^B^E∨F")
tree.insert("C^D")
println(tree.root(1).expression)
println(tree.root(1).childs(0).expression  + ", " + tree.root(1).childs(1).expression)
