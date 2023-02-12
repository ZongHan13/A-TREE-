
class Node(_expression: String) {
    var expression: String = _expression
    var result: Boolean = false
}


def stringToSet(text: String): Set[String] = {
    text.split("").toSet
}

def expressionToSet(text: String): Set[String] = {
    text.split('^').toSet
}

def findMaxIntersectSet(collection: List[Set[String]]): Set[String] = {
    val collection_for_process = collection.sortBy(x => x.size > x.size)
    //collection_for_process.foreach(x => println(x.mkString(",")))
    var most_curr_intersect: Set[String] = Set.empty
    for(i <- 0 until collection_for_process.size) {
        if (i == 0) { 
            val curr_intersect = collection(i).intersect(collection_for_process(i+1)) 
            if( curr_intersect.nonEmpty) {
            most_curr_intersect = curr_intersect
            
            }
        } else {
            val curr_intersect = most_curr_intersect.intersect(collection_for_process(i))
            if( curr_intersect.nonEmpty) {
                most_curr_intersect = curr_intersect
            }
        }
    } 
    most_curr_intersect
}





//val expressionSet = Array("A^B^D^E", "B^C^D^E", "D^F^E^G")
//val expressionSet = Array("A^B^D^E", "B^C^D^E", "X^Y^Z")
val expressionSet = Array("A^B^D^E", "B^C^D^E", "X^Y^Z","D^F^E^G", "X^G^B", "L^S^D","A^D^B^G")
val expressionsetssss = expressionSet.map(x => expressionToSet(x)).toList.sortBy(x => x.size)(ord = Ordering[Int].reverse)
val set_A = expressionToSet(expressionSet(0))
val set_B = expressionToSet(expressionSet(1))
val set_C = expressionToSet(expressionSet(2))
val set_D = expressionToSet(expressionSet(3))
//println(set_A.mkString(", "))
val set_collection = List(set_A, set_B, set_C, set_D)

// //println(set_A.intersect(set_B).intersect(set_C).intersect(set_D))
// println(expressionsetssss)
// //println(findMaxIntersectSet(set_collection))
// //println(findMaxIntersectSet(expressionsetssss))
// for( i <- 0 until expressionsetssss.size -1) {
//     print(s"Round$i: "  )
//     println(findMaxIntersectSet(expressionsetssss.dropRight(i)))
// }

// println("-----For diff test-----")
// val set_forDiff = set_collection
// println("-----Origin-----")
// println(set_forDiff.mkString(" "))
// println("----After diff with Set(D)")
// val set_diff = Set("D","C","E")
// println(set_forDiff.map(x => x.diff(set_diff)))


def findIntersectInMultipleSets(data: List[Set[String]]): Set[String] = {
    
    var curr_intersect = Set.empty[String]
    
    for(i <- 0 until data.size -1) {
        //println(data(i))
        //println(data(i+1))
        if (i == 0) {curr_intersect = data(i).intersect(data(i+1))}
        else if (curr_intersect.intersect(data(i+1)).nonEmpty && curr_intersect.intersect(data(i+1)).size > 1){curr_intersect = curr_intersect.intersect(data(i+1))}
        // else if (curr_intersect.intersect(data(i+1)).nonEmpty && curr_intersect.intersect(data(i+1)).size > curr_intersect.size) { 
        //     curr_intersect = curr_intersect.intersect(data(i+1))
        //     //data.map(x => x.diff(curr_intersect))
        
        //}
        
    }
    curr_intersect
}
// val rest = findIntersectInMultipleSets(expressionsetssss)

//  println(findIntersectInMultipleSets(expressionsetssss))
// val rest_1 = expressionsetssss.map(x => x.diff(rest)).sortBy(x => x.size)(Ordering[Int].reverse)
// println(rest_1)
// println(findIntersectInMultipleSets(rest_1))
// //val a = "shfdghdtrewgdhfghrt".split("").toSet
// //print(a)


val set_1 = List("sdfdghrtsfgdfg","qwhghkdfogretn", "scgdrtwrwerd", "dflhgmrtyt", "zcxflrmyoy").map(x => stringToSet(x))
val set_2 = set_1.sortBy(x => x.size)(Ordering[Int].reverse)
println(set_2)
println("---------------------")
val for_diff2 = findIntersectInMultipleSets(set_2)
println(for_diff2)
//println(for_diff2)
val set3 = set_2.map(x => x.diff(for_diff2)).sortBy(x => x.size)(Ordering[Int].reverse)
println("--------------------")
println(set3)
val for_diff3 = findIntersectInMultipleSets(set3)
println(for_diff3)
println("--------------------")
val set4 = set3.map(x => x.diff(for_diff3)).sortBy(x => x.size)(Ordering[Int].reverse)
println(set4)
println(findIntersectInMultipleSets(set4))
// //println(findMaxIntersectSet(set_2))
// val s1 = Set(3, 4, 5, 6, 7)
// val s2 = Set(1, 2, 3, 4, 5)
// val s3 = Set(5, 6, 7, 8, 9)

// println(s1.intersect(s2).intersect(s3))
// println(s3.intersect(s2).intersect(s1))
val node_aaa = new Node("aaa")
val arr1 = Array(node_aaa)
val arr2 = Array(node_aaa)
println("aar1's node_aaa value : " + arr1(0).expression + " " + node_aaa.result)
println("aar2's node_aaa value : " + arr2(0).expression + " " + node_aaa.result)


println("---Change node_aaa's value to bbb ----")
node_aaa.expression = "bbb1"
node_aaa.result = true
println("aar1's node_aaa value : " + arr1(0).expression + " " + node_aaa.result)
println("aar2's node_aaa value : " + arr2(0).expression + " " + node_aaa.result)
