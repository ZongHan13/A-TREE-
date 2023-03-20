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

class switch {
    var name: String = _
    var target_Node: ListBuffer[Node] = ListBuffer[Node]()
    var value_List: ListBuffer[String] = ListBuffer[String]()
}




class ATree() {

    var hen: HashMap[Int, Node] = HashMap[Int, Node]()
    var root: ListBuffer[Node] = ListBuffer[Node]()

    def insert(_expression: String): Node = {
        val id = generateID(_expression)
        println(_expression + id)
        if(hen.getOrElse(id,0) != 0) {
            hen(id).useCount += 1
            hen(id)
        } else {
            var childExprs = reorganize(_expression)
            var flag = true
            if (childExprs.isEmpty) {
                childExprs = List(_expression)
                flag = false
            }
            // println("!!!!!!!!!!!!!!!!!!!!!!!"+childExprs + "!! "+ _expression)
            // println("flag: " + flag)
            var childNodes: ListBuffer[Node] =  ListBuffer[Node]()
            //var childExprs = List(_expression)
            if(_expression.length() >1){
                if (childExprs.size > 1 ) {
                    for(expr <- childExprs) {
                        var childNode = insert(expr)
                        childNodes += childNode
                    }
                } else {
            for (childExpr <- childExprs) {
                
                //val childNode = insert(childExpr)
                //childNodes += childNode
                for (expr <- childExpr) {
                    if (expr != '^') {
                        var childNode = insert(expr.toString())
                        //insert(expr.toString())
                        childNodes += childNode
                        //println(expr)
                    }
                    //println(expr)
                }
                //println(childExpr)
            }
                }
            }
            val node = createNewNode(_expression,childNodes.toList)
            node.useCount += 1
            node.childExprs += childExprs
            
            if(flag == true) {selfAdjust(node)}
            hen += (id -> node)
            node
        }
        
    }

    
    
    
    private def createNewNode(expr: String, childNodes:List[Node]):Node = {
        val id = generateID(expr)
        val node = if(!expr.contains('^')) {
            new leaf_Node(expr)
        } else {
            new inner_Node(expr)

        }
        for(childNode <- childNodes) {
           node.childs += childNode
           childNode.parent += node
        }
        //node.childs += childNodes
        hen += (id -> node)
        node
    }




    private def generateID(_expression: String): Int = {
            val predicatesAndOperators: List[Char] = _expression.toList 
            var id: Int = 0
            for (char <- predicatesAndOperators) {
                id += char.hashCode() * char.hashCode()
            }
            if (predicatesAndOperators.contains('∨')) {
                id = Math.pow(id, 2).toInt
            }
            id
        }


    private def reorganize(_expression: String):List[String] = {
        //println("here")
        var u  = ListBuffer(List(_expression)).map(_.flatMap(_.split('^')).toSet).toSet
        //var c : Set[Set[String]] = Set.empty
        var c: ListBuffer[String] = ListBuffer[String]()
        var round = 0

        breakable{
            while (u.nonEmpty) {
            
                val s = selectAn_S_that_maximizes_insect_in_Hen(u, _expression)
                //println("s: " + s)
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
            // hen(generateID(_expression)).childExprs.clear()
            // hen(generateID(_expression)).childExprs.append(c.toList)
            }
            val foruni = u.map(x =>x.toList).toList
            //println("c === =!!!!" + c)
            c.appendAll(foruni.flatten).toList
        
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



    def selfAdjust(newNode:Node):Unit = {
        var childNodes = newNode.childs
        
         
        // for (childNode <-  childNodes) {
        //     //val childNodeforsearch = childNode.parent newNode
        //     for (parentNode <- childNode.parent) {
        //         if (parentExpressionContainsNewExpression(parentNode.expression, newNode.expression)) {
        //             println("current node:" + newNode.expression)
        //             println("parentNode:" + parentNode.expression)
        //             println("childNode:" + childNode.expression)
        //             parentNode.childs -= childNode
        //             if (!parentNode.childs.contains(newNode)){ parentNode.childs += newNode }
        //             childNode.parent -= parentNode
        //             //childNode.parent += node
        //             //node.childs += childNode
        //         }
        //     }
        // }
        // for (i <- 0 until childNodes.size) {
        //     for (j <- 0 until childNodes(i).parent.size-2) {
        //         if (parentExpressionContainsNewExpression(childNodes(i).parent(j).expression, newNode.expression)) {
        //             // println("current node:" + newNode.expression)
        //             // println("parentNode:" + parentNode.expression)
        //             // println("childNode:" + childNode.expression)
        //             childNodes(i).parent(j).childs -= childNodes(i)
        //             if (!childNodes(i).parent(j).childs.contains(newNode)){ childNodes(i).parent(j).childs += newNode }
        //             childNodes(i).parent -= childNodes(i).parent(j)
        //             //childNode.parent += node
        //             //node.childs += childNode
        //         }
        //     }
        // }
            for (i <- 0 until childNodes.size) {
                var j = 0
                while (j  < childNodes(i).parent.size -1) { ////// 這裡應該要從parent 先去掉 newNode自己
                    // 但-1不就是去掉自己嗎 因為一定會是在最後一個
                    if (parentExpressionContainsNewExpression(childNodes(i).parent(j).expression, newNode.expression)) {
                    // println("current node:" + newNode.expression)
                    // println("parentNode:" + parentNode.expression)
                    // println("childNode:" + childNode.expression)
                        childNodes(i).parent(j).childs -= childNodes(i)
                        if (!childNodes(i).parent(j).childs.contains(newNode)){ childNodes(i).parent(j).childs += newNode }
                        childNodes(i).parent -= childNodes(i).parent(j)
                    //childNode.parent += node
                    //node.childs += childNode
                    
                    }
                    j +=1
            }
        }

    }

    def parentExpressionContainsNewExpression(parentExpression: String, newExpression: String): Boolean = {
        val parentSet = exprToStringSet(parentExpression)
        val newSet = exprToStringSet(newExpression)
        if (newSet.subsetOf(parentSet)) {
            true
        } else {
            false
        }
    }

    
    

}

val tree = new ATree()
tree.insert("A^D^E^F")
tree.insert("A^B^C")
//println("1")
tree.insert("C^B^Z^X")
//println("2")
tree.insert("A^B^C^D")
tree.insert("A^B^Z^X")
tree.insert("A^B^Z")
tree.insert("A^B^Z^Y")

tree.insert("C^Z^E^F")
tree.insert("C^Z^E^F^G")
tree.insert("C^Z^E^F^A^B") 

//some index problem 
//tree.insert("C^Z^E^F^G^A^D")
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
// println("=================================")
// tree.hen.foreach(x => println(x._2.expression))
// println("=================================")
// println(tree.hen(592).expression)
// println(tree.hen(592).childExprs.size)
// println(tree.hen(592).childs(0).expression)
// println(tree.hen(592).childs(1).expression)
// println(tree.hen(592).childs(0).childs.size)
// println(tree.hen(592).childs(0).childs(0).expression)
// println(tree.hen(592).childs(0).childs(1).expression)
// println(tree.hen(592).childs(0).childs(2).expression)
// println(tree.hen(592).childExprs)
println("----------result--------------")

println(tree.hen(75011).expression)
println(tree.hen(75011).childs.size)
println(tree.hen(75011).childs(0).expression)
println(tree.hen(75011).childs(1).expression)

// println(tree.hen(45018).childs(2).expression)
// println(tree.hen(45018).childs(3).expression)
// println(tree.hen(45018).childs(3).childs.size)
println("------------------------")

println(tree.hen(44202).expression)
println(tree.hen(44202).childs.size)
println(tree.hen(44202).childs(0).expression)
println(tree.hen(44202).childs(1).expression)

println("------------")
println(tree.hen.size)
// for ((id, node) <- tree.hen) {
//     println(id + ":" + node.expression)
// }
println("Node A parents :")
tree.hen(4225).parent.foreach(x => println(x.expression))
println(" ----- Node A ------")
println(tree.hen(30742).expression)
println(tree.hen(30742).childs(0).expression)
println(tree.hen(30742).childs(1).expression)
//println(tree.hen(30742).childs(2).expression)
println("-------------")
// println(tree.hen(30742).childs(1).childs(0).expression)
// println(tree.hen(30742).childs(1).childs(1).expression)
println(tree.hen(17417).expression)
println(tree.hen(17417).parent(0).expression)
println(tree.hen(17417).childs.size)
println(tree.hen(17417).childs(0).expression)
println(tree.hen(17417).childs(1).expression)



println(tree.hen(75011).expression)
//println(tree.hen(75011).parent(0).expression)
println(tree.hen(75011).childs.size)
println(tree.hen(75011).childs(0).expression)
println(tree.hen(75011).childs(1).expression)

// val sorted_Map = tree.hen.toSeq.sortWith(_._1 < _._1)
// sorted_Map.foreach(x => println(x._1 + ":" + x._2.expression))

// println(tree.hen(51197).childs(2).expression)
// println(tree.hen(51197).childs(3).expression)

// println(tree.hen(62635).expression)
// println(tree.hen(62635).childs.size)
// println(tree.hen(62635).childs(0).expression)
// println(tree.hen(62635).childs(1).expression)
// println(tree.hen(51197).childs(2).expression)
// println(tree.hen(51197).childs(3).expression)
// println(tree.hen(51197).childs(3).childs.size)
// println("------------------------")
// println(tree.hen(591).expression)
// println(tree.hen(591).childExprs.size)
// println(tree.hen(591).childs(0).expression)
// println(tree.hen(591).childs(1).expression)
// println(tree.hen(591).childs(0).childs.size)
// println(tree.hen(591).childs(0).childs(0).expression)
// println(tree.hen(591).childs(0).childs(1).expression)
// println(tree.hen(591).childs(1).childs.size)
// println(tree.hen(591).childs(1).childs(0).expression)
// println(tree.hen(591).childs(1).childs(1).expression)

// println(tree.hen(591).childExprs)
//println(tree.hen(548).childs(1).expression)
//println(tree.hen(591).childs(2).expression)
// println(tree.hen(591).childs(3).expression)
// println(tree.hen(591).childs(4).expression)
// println(tree.hen(591).childs(5).expression)
// println(tree.hen(591).parent.size)
//println(tree.root(1).childs.size)
//println(tree.root(0).childs(0).expression)
// println("---------------")
// println(tree.root(1).expression)
// println(tree.root(1).childExprs.size)
// println(tree.root(1).childExprs)
// println(tree.root(1).childs.size)
// println(tree.root(2).childs(0).childExprs)
// println(tree.root(2).childs(0).childs.size)
//println(tree.root(2).childs(1).expression)
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