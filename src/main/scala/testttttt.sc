import scala.collection.mutable.Stack

def evaluateExpression(exprList: List[String], values: Map[String, Boolean]): Boolean = {
  val stack = Stack[Boolean]()
  
  stack.push(values(exprList(6)))
  stack.push(values(exprList(4)))
  stack.push(values(exprList(2)))
  stack.push(values(exprList(0)))
  val operator = List("^", "^","∨")
  for (token <- operator) {
    token match {
      case "^" => {
        val op1 = stack.pop()
        val op2 = stack.pop()
        stack.push(op1 && op2)
      }
      case "∨" => {
        val op1 = stack.pop()
        val op2 = stack.pop()
        stack.push(op1 || op2)
      }
      //case pred => stack.push(values(pred))
    }
  }
  
  if (stack.size != 1) {
    throw new IllegalArgumentException("Invalid expression")
  }
  
  stack.pop()
}

val exprList = List("BTC.value>1", "^", "ETH.value>2", "^", "XRP.value>3", "∨", "LTC.value>4")
val values = Map("BTC.value>1" -> true, "ETH.value>2" -> true, "XRP.value>3" -> false, "LTC.value>4" -> true)

val result = evaluateExpression(exprList, values)
// println(result)
var resutlttt = false
for(i <- 1 to exprList.length -1 by 2) {
    //var resutlttt = false
    if(exprList(i) == "^" && i ==1) {
        resutlttt = values(exprList(i-1)) && values(exprList(i+1))
        println("1")
    } else if (exprList(i) == "∨" && i == 1) {
        resutlttt = values(exprList(i-1)) || values(exprList(i+1))
        println("2")
    } else if (exprList(i) == "^") {
        resutlttt = resutlttt && values(exprList(i+1))
        println("3")
    } else if (exprList(i) == "∨") {
        resutlttt = resutlttt || values(exprList(i+1))
        println("4")
    }
    
}
println(resutlttt)