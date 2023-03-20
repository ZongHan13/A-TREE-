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
  var useCount: Int = 0
  var childExprs: ListBuffer[List[String]]  = ListBuffer[List[String]] ()
  var result: Boolean = _
  
  
//   private def evaluating():Boolean = {
//     val regex = "(?<!\\^)(?=\\^)|(?<=\\^)(?!\\^)|(?<!∨)(?=∨)|(?<=∨)(?!∨)"
//     val resultList = expression.split(regex).toList
    

//   }

}

class leaf_Node(_expression: String) extends Node {
  var expression: String = _expression
  var parent: ListBuffer[Node] = ListBuffer[Node]()
  var childs: ListBuffer[Node] = ListBuffer[Node]()
  var useCount: Int = 0
  var childExprs: ListBuffer[List[String]]  = ListBuffer[List[String]] ()
  var result: Boolean = false
}

class ATree() {

    var hen: HashMap[Int, Node] = HashMap[Int, Node]()
    var root: ListBuffer[Node] = ListBuffer[Node]()

    def insert(_expression: String): Node = {
        val id = generateID(_expression)
        println(_expression + "   " + id)
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
            if(_expression.length() >1) {
                if (childExprs.size > 1 ) {
                  for(expr <- childExprs) {
                    var childNode = insert(expr)
                        childNodes += childNode
                    }
               } 
            //else {
            // for (childExpr <- childExprs) {
                
            //     //val childNode = insert(childExpr)
            //     //childNodes += childNode
            //     for (expr <- childExpr) {
            //         if (expr != '^') {
            //             var childNode = insert(expr.toString())
            //             //insert(expr.toString())
            //             childNodes += childNode
            //             //println(expr)
            //         }
            //         //println(expr)
            //     }
            //     //println(childExpr)
            // }
            //     }
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
        println("U :" + u.mkString(", "))
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
            val interSet = set1.intersect(exprToPredicateSet(node.expression))
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

    def exprToPredicateSet(expr: String): Set[String] = {
        var stringSet: Set[String] = Set.empty
        println("expr :  "+expr)
    //     if (expr.length >1) {
    //     for (char <- expr) {
    //         stringSet += char.toString()
    //     }
    // }
        //println("HERE =: "+ stringSet)
        for(predicate <- expr.split('^')) {
            stringSet += predicate
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
                val interSet = target.intersect(exprToPredicateSet(node.expression))
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
        val parentSet = exprToPredicateSet(parentExpression)
        val newSet = exprToPredicateSet(newExpression)
        if (newSet.subsetOf(parentSet)) {
            true
        } else {
            false
        }
    }

    
    

}

val tree = new ATree()
tree.insert("BTC.value>1^ETH.value>2^XRP.value>3")
//println("1")
tree.insert("BTC.value>1^ETH.value>2^XRP.value>3^LTC.value>4")
//println("2")
// tree.insert("BTC.value>3^ETH.value>2^XRP.value>3^LTC.value>4^BCH.value>5")
tree.insert("BTC.value>1^ETH.value>2^XRP.value>3^LTC.value>4^BCH.value>5^EOS.value>6")

println("---------------- result ----------------")
println(tree.hen(367193).expression)
println(tree.hen(367193).childExprs)
println(tree.hen(367193).childs.size)
println(tree.hen(367193).childs(0).expression)
println(tree.hen(367193).childs(1).expression)

println(tree.hen(273485).parent(0).parent(0).expression)