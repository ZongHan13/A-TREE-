import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._

trait  Node {
    var expression: String
    var parent: ListBuffer[Node] 
    var childs : ListBuffer[Node] 
    var useCount: Int
    var childExprs: ListBuffer[List[String]] 
}

class inner_Node(_expression: String) extends Node {
  var expression: String = _expression
  var parent: ListBuffer[Node] = ListBuffer[Node]()
  var childs: ListBuffer[Node] = ListBuffer[Node]()
  var useCount = 0
  var childExprs: ListBuffer[List[String]]  = ListBuffer[List[String]] ()
}

class leaf_Node(_expression: String) extends Node {
  var expression: String = _expression
  var parent: ListBuffer[Node] = ListBuffer[Node]()
  var childs: ListBuffer[Node] = ListBuffer[Node]()
  var useCount = 0
  var childExprs: ListBuffer[List[String]]  = ListBuffer[List[String]] ()
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
                    childExprs.foreach(x => hen(id).childExprs += List(x.toString()))
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
                    val childPredicates = _expression.replace('^',' ').split(' ').toList
                    val childExprs = List(_expression)
                    
                    hen(id).childExprs += childExprs
                if (_expression.length() > 1) {
                
                    reorganize(_expression)
                    if(hen(id).childExprs.size > 1) {
                        for(expr <- hen(id).childExprs) {
                            val childNode = insert(expr(0))
                            hen(id).childs += childNode
                            //childNode.parent += hen(id)
                            }
                        } else {
                        for(expr <- hen(id).childExprs) {
                            if (expr.size == 1) {
                            for (ss <- expr) {
                                for (s <- ss) {
                                    if (s != '^') {
                                        val childNode = insert(s.toString())
                                        hen(id).childs += childNode
                                        //childNode.parent += hen(id)
                                    }
                                }
                                //childNode.parent += hen(id)
                                }
                            } else {
                            for (s <- expr) {
                                val childNode = insert(s)
                                hen(id).childs += childNode
                                //childNode.parent += hen(id)
                            }
                            }
                            //childNode.parent += hen(id)
                            }
                    }


                    } 
                    //
                    val node = createNewNode(expr = _expression)
                    node.useCount = 1
                    selfAdjust(node)
                    hen += (id -> node)
            
                } else {
                    val new_node = new leaf_Node(_expression)
                    hen += (id -> new_node)
                }
            }
        }
       
        hen(id)
    }

    private def createNewNode(expr: String):Node = {
        val id = generateID(expr)
        val node = if(!expr.contains('^')) {
            new leaf_Node(expr)
        } else {
            new inner_Node(expr)
        }
        node
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
        //println("here")
        var u  = hen(generateID(_expression)).childExprs.map(_.flatMap(_.split('^')).toSet).toSet
        //var c : Set[Set[String]] = Set.empty
        var c: ListBuffer[String] = ListBuffer[String]()
        var round = 0

        breakable{
            while (u.nonEmpty) {
            
                val s = selectAn_S_that_maximizes_insect_in_Hen(u, _expression)
                println("s: " + s)
                if( s.isEmpty ) {
                
                    break()
                }
                u = u.map(_.filter(!s.contains(_)))
                println("u: " + u)
                c += setRecoveryToStringWithAnd(s)
                println("c: " + c)
                round += 1
            }
            
        }   
            if (round > 1) {
            hen(generateID(_expression)).childExprs.clear()
            hen(generateID(_expression)).childExprs.append(c.toList)
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

    def setRecoveryToStringWithAnd(set: Set[String]): String = {
        var string = ""
        for (s <- set) {
            string += s + "^"
        }
        string = string.substring(0, string.length() - 1)
        string
    }

    // def selectAn_S_that_maximizes_insect_in_Hen(_expression: String): Set[String] = {
    //     var childExprs_Set = hen(generateID(_expression)).childExprs.map(_.flatMap(_.split('^')).toSet).toSet
    //     var maxinterSet: Set[String] = Set.empty
    //     val hen_no_self = hen - generateID(_expression)
    //     for (target <- childExprs_Set) {
    //         for ((id,node) <- hen_no_self) {
    //             val interSet = target.intersect(exprToStringSet(node.expression))
    //             if (interSet.size > maxinterSet.size) {
    //                 maxinterSet = interSet
    //             }
    //         }
    //     }
    //     maxinterSet
    // }

     def selectAn_S_that_maximizes_insect_in_Hen(target_set: Set[Set[String]], _expression:String): Set[String] = {
        //var childExprs_Set = hen(generateID(_expression)).childExprs.map(_.flatMap(_.split('^')).toSet).toSet
        var maxinterSet: Set[String] = Set.empty
        val hen_no_self = hen - generateID(_expression)
        for (target <- target_set) {
            for ((id,node) <- hen_no_self) {
                val interSet = target.intersect(exprToStringSet(node.expression))
                if (interSet.size > maxinterSet.size) {
                    maxinterSet = interSet
                }
            }
        }
        maxinterSet
    }



    def selfAdjust(node:Node):Unit = {
        val childNodes = node.childs
        for (childNode <- childNodes) {
            for (parentNode <- childNode.parent) {
                 {
                    // parentNode.childExprs -= List(childNode.expression)
                    // parentNode.childs -= childNode
                    // parentNode.childExprs += List(hen(generateID(new_Node_expr)).expression)
                    // parentNode.childs += hen(generateID(new_Node_expr))   
                    //buggy 
                }
            }
        }
    }

    
    

}

val tree = new ATree()
tree.insert("A^B^E^F")
tree.insert("C^D^Z^X")
tree.insert("A^B^C^D")
// tree.insert("A^B^C")
// tree.insert("C^Z^E^F")
// println(tree.root(2).expression)
// println(tree.root(2).childExprs.size)
// tree.root(2).childExprs.foreach(x => println(x))
// println(tree.root(2).childs.size)
// println(tree.root(2).childs(0).expression)
// println(tree.root(2).childs(0).parent.size)
// println(tree.root(2).childs(0).parent(0).expression)
// println(tree.root(2).childs(0).parent(1).expression)
// println(tree.root(2).childExprs.mkString(","))
// //println(tree.root(0).childExprs)
// // println(tree.root(0).childs(0).parent(0).expression)
// // println(tree.root(0).childs(1).expression)
// println("-----------------------------")
// println(tree.root(4).childs.size)
// println(tree.root(4).childs(0).expression)
// println(tree.root(4).childs(0).parent.size)
//println(tree.hen.size)
//tree.hen.foreach(x => println(x._2.expression))
// println(tree.root(0).expression)
// println(tree.root(0).childExprs.size)
// println(tree.root(0).childExprs)
//println(tree.root(1).childs.size)
//println(tree.root(0).childs(0).expression)
println("---------------")
println(tree.root(1).expression)
println(tree.root(1).childExprs.size)
println(tree.root(1).childExprs)
println(tree.root(1).childs.size)
println(tree.root(1).childs(0).expression)
println(tree.root(1).childs(1).expression)
// println(tree.root(2).childs(2).expression)
// println(tree.root(2).childs(3).expression)
// println(tree.root(0).childs.size)

// println(tree.root(0).childs(1).expression)
// println(tree.root(0).childs(1).parent.size)
// println(tree.root(0).childs(1).parent(0).expression)
// println(tree.root(0).childs(1).parent(1).expression)
// //println(tree.root(2).childExprs.mkString(","))
// println("-----------------------------")
// println(tree.root(0).childs.size)
// println(tree.root(0).childs(2).expression)
// println(tree.root(0).childs(2).parent.size)
// println(tree.root(0).childs(2).parent(0).expression)
// println(tree.root(0).childs(2).parent(0).expression)
// //println(tree.root(2).childExprs.mkString(","))
// println("-----------------------------")
// println(tree.root(0).childs.size)
// println(tree.root(0).childs(3).expression)
// println(tree.root(0).childs(3).parent.size)
// println(tree.root(0).childs(3).parent(0).expression)
// println(tree.root(0).childs(3).parent(0).expression)
// //println(tree.root(2).childExprs.mkString(","))
// println("-----------------------------")

//println(tree.root(0).childs(0).childs.size)
// println(tree.root(0).childs(0).childs(0).parent(0).expression)
// println(tree.root(0).childs(0).childs(1).parent(0).expression)
// println(tree.root(0).childs(0).childs(2).parent(0).expression)
//println(tree.root(0).childs(1).expression)