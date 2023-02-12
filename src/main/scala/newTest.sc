
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

val root_map = scala.collection.mutable.Map[String, Int]()
val leaf_map = scala.collection.mutable.Map[String, Int]()

val second_layer_map = scala.collection.mutable.Map[String, Int]()
var second_layer_set = Set[Set[String]]()

var inner_expression: ListBuffer[String] = new ListBuffer[String]()


val root_Node_map = scala.collection.mutable.Map[String, root_Node]()
val inner_Node_map = scala.collection.mutable.Map[String, inner_Node]()
val leaf_Node_map = scala.collection.mutable.Map[String, leaf_Node]()

class root_Node(_expression: String) {
  var expression: String = _expression
  var child: List[inner_Node] = _
}

class inner_Node(_expression: String) {
  var expression: String = _expression
  var parent: List[root_Node] = _
  var child: List[leaf_Node] = _
}

class leaf_Node(_expression: String) {
  var expression: String = _expression
  var parents: List[leaf_Node] = _
}





def parse_root(queries: Array[String]): Unit = {
  

  for (query <- queries) {
    root_map(query) = root_map.getOrElse(query, 0) + 1
    root_Node_map(query) = root_Node_map.getOrElse(query, new root_Node(query))
  }
 


  for (query <- queries) {
    val predicates = query.split("[^A-Z]+").filter(_.nonEmpty)
    for (predicate <- predicates) {
      leaf_map(predicate) = leaf_map.getOrElse(predicate, 0) + 1
      leaf_Node_map(predicate) = leaf_Node_map.getOrElse(predicate, new leaf_Node(predicate))
    }
  }

  
} // this function will count the each query and each predicate then get a Map e.g. (A -> 1 , B -> 1, C ->1, A^B^C ->1)

//ok so now i need a function that can parse disjunctive normal form
def parse_root_to_second(queries: Array[String]): Unit = {
  for (query <- queries) {
    if (query.contains("∨")) {
      query.split("∨").foreach(x =>  
        if (x.length() > 1) {
        second_layer_map(x) = second_layer_map.getOrElse(x, 0) +1
        second_layer_set += x.split('^').toSet
        } 
      )
    } else {
      second_layer_map(query) = second_layer_map.getOrElse(query, 0) +1
      second_layer_set += query.split('^').toSet
    }
  }
}





val test_data = Array("A^B^D","A^D^B", "C^D^E", "B^D^F")
val test_data1 = Array("A^B^D","A^D^B", "C^D^A", "B^D∨F", "C^D^G")
val test_data2 = Array("A^B^D","A^D^B", "C^D^A", "B^D∨F", "C^D^G", "A^G^C", "C^F^A")
val test_data3 = Array("A^B^D^E","A^B^E","B^D^E")
val test_data4 = Array("d^e^f^g^Z^D^B".toUpperCase(), "d^b^q^p^j^g".toUpperCase(),"u^q^a^Z^D^B".toUpperCase(), " j^a^k^Z^D^B".toUpperCase(), "w^e^h^u^i^k".toUpperCase()
, "d^e^f^g^z^y^q".toUpperCase(), "d^e^f^g^z^k^l".toUpperCase(), "Z^D^Q","Z^D^E", "Z^D^B"

)
parse_root(test_data4)
parse_root_to_second(test_data4)


println("Root Map: " + root_map)
println("------------------------------------")
root_Node_map.foreach(x => println(x._1 + "Node:" + x._2.expression + ", Class: " + x._2.getClass()))
println("------------------------------------")
println("Second layer Map: " + second_layer_map)
println("------------------------------------")
println("second layer Set: " + second_layer_set)
println("------------------------------------")
println(leaf_map)
leaf_Node_map.foreach(x => println(x._1 + " Node:" +   x._2.expression + ", Class: " + x._2.getClass()))
println("------------------------------------")
// val testttt = "B^D∨F"

val aaaaa = leaf_map.toList.sortBy(x => x._2)(ord = Ordering[Int].reverse)
println(aaaaa)



// for(i <- 0 until aaaaa.length -1) { 
//   breakable {
//   for( expression <- second_layer_map) {
//       if( expression._1.contains(aaaaa(i)._1) & expression._1.contains(aaaaa(i+1)._1)) {
//         inner_expression += (aaaaa(i)._1 + aaaaa(i+1)._1)
//         // if(expression._1.contains(aaaaa(i)._1) & expression._1.contains(aaaaa(i+1)._1) & expression._1.contains(aaaaa(i+2)._1)) {
//         // //println(aaaaa(i)._1 + aaaaa(i+1)._1 + aaaaa(i+2)._1)
//         // inner_expression(i) = aaaaa(i)._1 + aaaaa(i+1)._1 + aaaaa(i+2)._1
//         // }
//         break()
//       }
//     }
//   }
// }
// println(inner_expression)


var level_1_List = ListBuffer[Set[String]]()
var level_1_Set = Set[Set[String]]()
var j = 0 



while (j < aaaaa.length -1) {
  
  var flag1 = false
  for(set_ele <- second_layer_set) {
    if (set_ele.contains(aaaaa(j)._1) & set_ele.contains(aaaaa(j+1)._1) ) {
      flag1 = true
    }
  }
  if (flag1 == true) {
    level_1_Set += Set(aaaaa(j)._1, aaaaa(j+1)._1)
    j += 2
  } else {
    j += 1  
  }
  


}


println("level1:" +level_1_Set)
 
val leaf_List = leaf_map.keySet.toArray
var level_2_Set = Set[Set[String]]()
for(ele <- level_1_Set) {
  val for_diff = ele.toSeq
  val the_rest = leaf_List.diff(for_diff)
  
  var flag2 = false
  for( elee <- the_rest) {
    val fortestt = ele.toList :+ elee
    val forflag = fortestt
    for (query <- second_layer_set) {
      if (!query.toArray.diff(forflag).nonEmpty) {
        flag2 = true
      }
    }
    if (flag2 == true) {
    level_2_Set += ele + elee
    flag2 = false
  }
  flag2 = false
  }
  
}
println("level2: " +level_2_Set)



var level_3_Set = Set[Set[String]]()
var second_layer_set2 = second_layer_set
for(ele <- level_2_Set) {
  val for_diff = ele.toSeq
  val the_rest = leaf_List.diff(for_diff)
  
  var flag2 = false
  for( elee <- level_2_Set) {
    val fortestt = ele.toList :+ elee
    val forflag = fortestt
    for (query <- second_layer_set2) {
      if (!query.toArray.diff(forflag).nonEmpty) {
        second_layer_set2.dropWhile(x => x.equals(query))
        flag2 = true
      }
    }
    if (flag2 == true) {
    level_3_Set += ele.union(elee)
    flag2 = false
  }
  flag2 = false
  }
  
}
println("level3: " +level_3_Set)



var level_4_Set = Set[Set[String]]()
var second_layer_set3 = second_layer_set
for(ele <- level_2_Set) {
  val for_diff = ele.toSeq
  val the_rest = leaf_List.diff(for_diff)
  
  var flag2 = false
  for( elee <- level_3_Set) {
    val fortestt = ele.toList :+ elee
    val forflag = fortestt
    for (query <- second_layer_set3) {
      if (!query.toArray.diff(forflag).nonEmpty) {
        second_layer_set3.dropWhile(x => x.equals(query))
        flag2 = true
      }
    }
    if (flag2 == true) {
    level_4_Set += ele.union(elee)
    flag2 = false
  }
  flag2 = false
  }
  
}
println("level4: " +level_4_Set)


var level_5_Set = Set[Set[String]]()
var second_layer_set4 = second_layer_set
for(ele <- level_2_Set) {
  val for_diff = ele.toSeq
  val the_rest = leaf_List.diff(for_diff)
  
  var flag2 = false
  for( elee <- level_4_Set) {
    val fortestt = ele.toList :+ elee
    val forflag = fortestt
    for (query <- second_layer_set4) {
      if (!query.toArray.diff(forflag).nonEmpty) {
        second_layer_set4.dropWhile(x => x.equals(query))
        flag2 = true
      }
    }
    if (flag2 == true) {
    level_5_Set += ele.union(elee)
    flag2 = false
  }
  flag2 = false
  }
  
}
println("level5: " +level_5_Set)


var level_6_Set = Set[Set[String]]()
var second_layer_set5 = second_layer_set
for(ele <- level_2_Set) {
  val for_diff = ele.toSeq
  val the_rest = leaf_List.diff(for_diff)
  
  var flag2 = false
  for( elee <- level_5_Set) {
    val fortestt = ele.toList :+ elee
    val forflag = fortestt
    for (query <- second_layer_set5) {
      if (!query.toArray.diff(forflag).nonEmpty) {
        second_layer_set5.dropWhile(x => x.equals(query))
        flag2 = true
      }
    }
    if (flag2 == true) {
    level_6_Set += ele.union(elee)
    flag2 = false
  }
  flag2 = false
  }
  
}
println("level6: " +level_6_Set)




//println(leaf_List.mkString(","))

// val level1arr = level_1_Set.toArray
// val fortest1 = level1arr(0).union(level1arr(1))
// //println(fortest1)

// for(query <- second_layer_set) {
//   if( !query.toArray.diff(fortest1.toList).nonEmpty) {
//     level_2_Set += fortest1
//   }
// }
// println("level2: " + level_2_Set)

//println(the_rest_List.length)

// val a1 = Array("B","D","A","E")
// val a2 = Array("D","E")
// println(a1.diff(a2).mkString)
// val a3 = a1.diff(a2)
// println(a3.mkString(","))
// // val rell = disjunctive_parser(testttt)
// // println(rell)
// val ss1 = Set("a","b","d","e")
// val ss2=  Set("a","d")
// val ss3 = Set("b","e")


// def combinePredicates(predicates: List[String], counts: scala.collection.mutable.Map[String, Int], queries: List[String]): List[String] = {
//   val sortedCounts = counts.toList.sortBy(-_._2)

//   def combine(predicates: List[String], combinations: List[String]): List[String] = {
//     if (predicates.isEmpty) combinations
//     else {
//       val newCombinations = for {
//         combination <- combinations
//         predicate <- predicates
          
//         if queries.exists(query => query.contains(combination + "^" + predicate))
        
//       } yield combination + "^" + predicate
      
//       if (newCombinations.isEmpty) { 
        
//         combinations 
        
//       }
//       else combine(predicates.tail, newCombinations)
//     }
//   }

//   combine(sortedCounts.map(_._1), sortedCounts.map(_._1))
// }

// val forquerytest = test_data.toList
// val count = leaf_map
// val predicate = count.keySet.toList

// val cccccccccccc = combinePredicates(predicate, count, forquerytest)
// println(cccccccccccc)

// import scala.collection.mutable

