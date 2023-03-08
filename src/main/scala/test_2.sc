import scala.io.Source
import collection.mutable
import scala.reflect.ClassTag



abstract class Query
case class subQeury(from: String, pattern: String, value: String, period: String, and_or: String) extends Query
case class vshape(combo : Seq[subQeury]) extends Query

//need query id 
//case class subQuery(from: String, pattern: String, value: String, period: String, and_or: String, id: Stirng) extends Query

class testQuery {
  private var from_q:String = ""
  private var pattern_q: String = ""
  private var value_q: String = ""
  private var period_q: String = ""
  private var and_or_q: String = ""

  def setQuery(from: String, pattern: String, value: String, period: String, and_or: String) = {
    from_q = from
    pattern_q = pattern
    value_q = value
    period_q = period
    and_or_q = and_or



  }
  def showvalue:String = {
    and_or_q
  }
  
}

def createquery(arr: String): subQeury = {
  val query_arr = arr.split(",")
  val output = query_arr.map(x => {val y = x.split(":"); (y(0),y(1)) } ).map(x => (x._1,x._2)).toMap
  val q : subQeury = subQeury(output("From"), output("Pattern"), output("Value"), output("Period"), output("AND_OR"))
  q
}


def  createtestquery(arr: String): testQuery = {
  val query_arr = arr.split(",")
  val output = query_arr.map(x => {val y = x.split(":"); (y(0),y(1)) } ).map(x => (x._1,x._2)).toMap
  val q : testQuery = new testQuery()
  q.setQuery(output("From"), output("Pattern"), output("Value"), output("Period"), output("AND_OR"))
  q
}

val filename = "/home/pcdm/hann/test/Han.txt"

val txt = Source.fromFile(filename).mkString

val s = txt.split("\n")

val ss = s.map(x => x.replace("|",",").replace(" ",""))
val anyList = List[Any]()
val test = ss.map(x => createquery(x))
test.foreach(println)


val newMap = mutable.Map[String, Array[subQeury]]("Han" -> test)
println(newMap("Han")(1))
println(newMap.mkString)

var array_value = Array[subQeury]()

/*for(sub <- test) {
  if (sub.value.substring(1).toFloat > 0) {
    println(sub.value.substring(2))
  }
}*/

def divide(qq: subQeury): vshape = {
  val qq1 = subQeury(qq.from, "Down", qq.value.substring(0,2), (qq.period.substring(0,2).toFloat /2).toString + "s", qq.and_or)
  val qq2 = subQeury(qq.from, "UP", qq.value.substring(2), (qq.period.substring(0,2).toFloat /2).toString + "s", qq.and_or)
  vshape(Seq(qq1,qq2))

}

// //val a = divide(test(0))
// val b = test.indexWhere(element => element.pattern == "V-shape")
// val testt = test.drop(test.indexWhere(element => element.pattern == "V-shape")+1)
// println(a)
//output.foreach(println)


//val a = Seq[subQeury]()



//println(a.mkString)


//println(os.read(os.pwd/"".endsWith(".txt")))


