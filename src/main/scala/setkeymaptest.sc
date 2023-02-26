
class Node(_expr: String) {
    var expression: String = _expr
}

val set_map = scala.collection.mutable.Map[Set[String], String]()

set_map += (Set("a", "b") -> "ab")

set_map += (Set("b", "a") -> "ba")

println(set_map(Set("a", "b")))

println(set_map(Set("b", "a")))


val testList = List(new Node("A"), new Node("B"), new Node("C"), new Node("D"))
val fordiff = List(new Node("C"), new Node("D"))

println("")

testList.foreach(x => print(x.expression))

diff.foreach(x => print(x.expression))