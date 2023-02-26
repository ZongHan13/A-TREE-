
import scala.collection.mutable.HashMap

class Node(_expr: String) {
    var expression: String = _expr
    var child: Node = _
    def insert(_expr: String): Unit = {
        val id = generateID(_expr)
        hen += (id -> new Node(_expr))
        child = hen(id)
    }
}


def generateID(_expression: String): Int = {
            val predicatesAndOperators: List[Char] = _expression.toList 
            var id: Int = 0
            for (char <- predicatesAndOperators) {
                id += char.hashCode()
            }
            id
        }

var hen: HashMap[Int, Node] = HashMap[Int, Node]()

val id = generateID("A^B")
println(id)

val node_root = new Node("dummy")
node_root.insert("A^B")
println(node_root.child.expression)