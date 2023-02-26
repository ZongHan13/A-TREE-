import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._

trait  Node {
    var expression: String
    var parent: ListBuffer[Node] 
    var childs : ListBuffer[Node] 
    var useCount: Int
    var childExprs: ListBuffer[String] 
}

class inner_Node(_expression: String) extends Node {
  var expression: String = _expression
  var parent: ListBuffer[Node] = ListBuffer[Node]()
  var childs: ListBuffer[Node] = ListBuffer[Node]()
  var useCount = 0
  var childExprs: ListBuffer[String] = ListBuffer[String]()
}

class leaf_Node(_expression: String) extends Node {
  var expression: String = _expression
  var parent: ListBuffer[Node] = ListBuffer[Node]()
  var childs: ListBuffer[Node] = ListBuffer[Node]()
  var useCount = 0
  var childExprs: ListBuffer[String] = ListBuffer[String]()
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
            if (_expression.contains('∨')) {
                if (_expression.length() > 1) {
                    val new_node = new inner_Node(_expression)
                    hen += (id -> new_node)
                    root += hen(id)
                    val childPredicate = _expression.replace('^',' ').replace('∨',' ').split(' ').toList
                    val childExprs = _expression.split('∨')
                    childExprs.foreach(x => hen(id).childExprs += x.toString())
                if (_expression.length() > 1) {
                    for(expr <- childExprs) {
                    val childNode = insert(expr)
                    hen(id).childs += childNode
                    childNode.parent += hen(id)
                    }
                }
            
                } else {
                    val new_node = new leaf_Node(_expression)
                    hen += (id -> new_node)
                }
            } else {
                if (_expression.length() > 1) {
                    val new_node = new inner_Node(_expression)
                    hen += (id -> new_node)
                    root += hen(id)
                    val childPredicates = _expression.replace('^',' ').replace('∨',' ').split(' ').toList
                    val childExprs = _expression
                    hen(id).childExprs += childExprs
                if (_expression.length() > 1) {
                
                    reorganize(_expression)
                    for(predicate <- childPredicates) {
                        val childNode = insert(predicate.toString())
                        hen(id).childs += childNode
                        childNode.parent += hen(id)
                        }
                    }
                    selfAdjust(_expression)
            
                } else {
                    val new_node = new leaf_Node(_expression)
                    hen += (id -> new_node)
                }
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
        println("here")
        var u  = hen(generateID(_expression)).childExprs.map(x => x.split('^').toSet).toSet
        //var c : Set[Set[String]] = Set.empty
        var c: ListBuffer[String] = ListBuffer[String]()
        breakable{
        while (u.nonEmpty) {
            val s = selectAn_S_that_maximizes_insect_in_Hen(_expression)
            println("s: " + s)
            if( s.isEmpty ) {
                break()
            }
            u = u -- Set(s)
            println("u: " + u)
            c += setRecoveryToStringWithAnd(s)
            println("c: " + c)
            }
        }   
        hen(generateID(_expression)).childExprs ++= c
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

    def setRecoveryToStringWithAnd(set: Set[String]): String = {
        var string = ""
        for (s <- set) {
            string += s + "^"
        }
        string = string.substring(0, string.length() - 1)
        string
    }

    def selectAn_S_that_maximizes_insect_in_Hen(_expression: String): Set[String] = {
        var childExprs_Set = hen(generateID(_expression)).childExprs.map(x => x.split('^').toSet).toSet
        var maxinterSet: Set[String] = Set.empty
        for (target <- childExprs_Set) {
            for ((id,node) <- hen) {
                val interSet = target.intersect(exprToStringSet(node.expression))
                if (interSet.size > maxinterSet.size) {
                    maxinterSet = interSet
                }
            }
        }
        maxinterSet
    }

    def selfAdjust(new_Node_expr: String):Unit = {
        val childNodes = hen(generateID(new_Node_expr)).childs
        for (childNode <- childNodes) {
            for (parentNode <- childNode.parent) {
                if (parentNode.expression.split('^').toSet.contains( hen(generateID(new_Node_expr)).expression.split('^').toSet)) {
                    parentNode.childExprs -= childNode.expression
                    parentNode.childExprs += hen(generateID(new_Node_expr)).expression
                }
            }
        }
    }

    
    

}

val tree = new ATree()
tree.insert("A^B^E^F")
tree.insert("C^D^Z^X")
tree.insert("A^B^C^D")
println(tree.root(2).expression)
println(tree.root(2).childExprs.size)
tree.root(2).childExprs.foreach(x => println(x))
//println(tree.root(0).childExprs)
// println(tree.root(0).childs(0).parent(0).expression)
// println(tree.root(0).childs(1).expression)
println("-----------------------------")



//println(tree.root(0).childs(0).childs.size)
// println(tree.root(0).childs(0).childs(0).parent(0).expression)
// println(tree.root(0).childs(0).childs(1).parent(0).expression)
// println(tree.root(0).childs(0).childs(2).parent(0).expression)
//println(tree.root(0).childs(1).expression)