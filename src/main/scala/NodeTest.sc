import scala.collection.mutable.ListBuffer

class inner_Node(val expr: String) {
    var result: Boolean = false
    var parent: ListBuffer[inner_Node] = ListBuffer[inner_Node]()
    val child: ListBuffer[Any] = ListBuffer[Any]()
    var trueCounter = 0
    var resultCount = 0
    var comesNode: ListBuffer[Any] = ListBuffer[Any]()

    def addChild(node: Any):Unit = {
        node match {
            case n: inner_Node => {
                n.parent += this
                child += n
                resultCount += 1
            }
            case n: leaf_Node => {
                n.parent += this
                child += n
                resultCount += 1
            }
        }
        
        
    }
    def evaluating:Unit = {
        if(trueCounter ==  resultCount) {
            result = true 
            if (result == true)  {
                println(s"Sender $expr : Send notification to user")
                result = false
                trueCounter = 0
                comesNode.clear()
            }
            for (p <- parent) {
                if(!p.comesNode.contains(this)) {
                    p.comesNode += this
                    p.trueCounter +=1
                }
                p.evaluating
            }
        }
        else result = false
    }

   
}


class leaf_Node(val expr: String) {
    
    var result: Boolean = false
    var parent: ListBuffer[inner_Node] = ListBuffer[inner_Node]()
    def changeResult(value: String): Unit = {
        value match {
            case "true" => result = true ; propagate(result)
            //case "false" => result = false ; propagate(result)
            case _ => //do nothing
        }
  
    }

    def propagate(result: Boolean): Unit = {// propagate result to parent
        for (p <- parent) {
            if(!p.comesNode.contains(this)) {
                p.comesNode += this
                p.trueCounter +=1
            }
            p.evaluating
        }
    }
    

}


class switch() {
    var leaf_Node_list: ListBuffer[leaf_Node] = ListBuffer[leaf_Node]()

    def changeNodeResult(value: String) = {
        value match {
            case "true" => leaf_Node_list.foreach(_.changeResult("true"))
            case "false" => for (n <- leaf_Node_list) n.changeResult("false")
            case _ => //do nothing
        }
    }


    def addNodeToSwitch(node: leaf_Node): Unit = {
        leaf_Node_list += node
    }
}


def maxPosOrNeg(number: List[Int]): Int = {
    number.count(_ > 0).max(number.count(_ < 0))
}


def add10(x: Int): Int = {
    x + 10
}


val a  = new inner_Node("a")
val b = new leaf_Node("b")
val c = new leaf_Node("c")
val d = new inner_Node("d")
val e = new leaf_Node("e")
val switch_1 = new switch()
a.addChild(b)
a.addChild(c)
d.addChild(a)
d.addChild(e)
switch_1.addNodeToSwitch(b)
switch_1.addNodeToSwitch(c)
//println(a.resultCount)
// println(a.result)
switch_1.changeNodeResult("true")
// println("-------round 1----------")
//b.changeResult("true")
// println(a.trueCountr)
// println(a.result)
// println("---------round 2--------")

//c.changeResult("true")
// println(a.trueCountr)
println(a.result)
println(a.trueCounter)
println(a.resultCount)
println(a.comesNode.size)
println("-0---------------------")
e.changeResult("true")
// println(d.result)
// println(d.resultCount)
// println(d.trueCountr) 

val listt = List(-1,7,-2)
//println(add10(maxPosOrNeg(listt)))