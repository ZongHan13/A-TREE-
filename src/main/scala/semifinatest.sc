import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

val root_map = scala.collection.mutable.Map[String, Int]() //暫時沒用到
val leaf_count_map = scala.collection.mutable.Map[String, Int]() //紀錄leaf node的數量

val second_layer_map = scala.collection.mutable.Map[String, Int]() //目前沒用到
val second_layer_node_map = scala.collection.mutable.Map[String, Node]() // 存second layer node 
val second_layer_node_List = ListBuffer[Node]() //用來把second layer node 連接到leaf node
var second_layer_set = Set[Set[String]]() //還沒用到



var inner_expression: ListBuffer[String] = new ListBuffer[String]() // 暫時用不到了

var inner_Node_Map_Set_key = scala.collection.mutable.Map[Set[String], inner_Node]()


val global_Node_map = scala.collection.mutable.Map[String, Node]() // 全局的node map
val root_Node_map = scala.collection.mutable.Map[String, root_Node]() //紀錄root node
val inner_Node_map = scala.collection.mutable.Map[String, inner_Node]() //目前沒用到
val leaf_Node_map = scala.collection.mutable.Map[String, leaf_Node]() //紀錄leaf node

trait Node {
  var expression: String 
  var child: ListBuffer[Node]
}


class root_Node(_expression: String) extends Node {
  var expression: String = _expression
  var child: ListBuffer[Node] = ListBuffer[Node]()
}

class inner_Node(_expression: String) extends Node {
  var expression: String = _expression
  var parent: List[root_Node] = _
  var child: ListBuffer[Node] = ListBuffer[Node]()
}

class leaf_Node(_expression: String) extends Node {
  var expression: String = _expression
  var parents: List[leaf_Node] = _
  var child: ListBuffer[Node] = ListBuffer[Node]()
}

class ATree() {
    var root: ListBuffer[root_Node] = ListBuffer[root_Node]()

    def insert(expression: String): Unit = {   //插入新的expression
        if (root_Node_map.getOrElse(expression, 0) == 0) {
            val new_node = new root_Node(expression)
            root += new_node
            //connect_leaf_to_root(split_to_leaf(expression)) //important function
            //combine_with_leaf_count(leaf_map)
            //split_to_leaf(expression)
            leaf_counter(expression)
            connect_root_to_second_layer(new_node)
            second_layer_node_map.foreach(x => connect_leaf_to_secondlayer(split_to_leaf(x._1)))
            
            if (root.size >1) {
                combine_leaf_as_inner(leaf_count_map)
            }


        } else {
            
        }
    }


    private def leaf_counter(expression: String): Unit = { //計算leaf node出現的次數
        //val leaf_predicate = expression.split('^') 
        val leaf_predicate = expression.replace('^',' ').replace('∨',' ').split(' ')
        for (i <- leaf_predicate) {
            if (leaf_count_map.getOrElse(i, 0) == 0) {
                leaf_count_map += (i -> 1)
            } else {
                leaf_count_map(i) += 1
            }
        }
        
       
    }

    
    private def split_to_leaf(expression: String): Array[String] = { //把expression分成leaf node
        val leaf_predicate = expression.split('^')
        for (i <- leaf_predicate) {
            if (leaf_count_map.getOrElse(i, 0) == 0) {
            } else {     
            }
        }
        
        for (i <- leaf_predicate) {
            if (leaf_Node_map.getOrElse(i, 0) == 0) {
                val new_node = new leaf_Node(i)
                leaf_Node_map += (i -> new_node)
            } else {
                
            }
        }
        leaf_predicate
    }
    private def connect_leaf_to_secondlayer(leaf_predicate: Array[String]): Unit = { //把leaf node連接到second layer node
        for (i <- leaf_predicate) {
            for (j <- second_layer_node_List) {
                if (j.expression.contains(i)) {
                    if(j.child.contains(leaf_Node_map(i))) {
                        
                    } else {
                        j.child += leaf_Node_map(i)
                    }
                    //j.child += leaf_Node_map(i)
                }
                    
                }
            }
        }

    private def combine_with_leaf_count(leaf_count : scala.collection.mutable.Map[String, Int]): Unit = { //還要修改目前用不了
        val leaf_count_sorted = leaf_count.toList.sortBy(_._2)(ord = Ordering[Int].reverse)
        
        if(leaf_count_sorted(0)._2 > 1 && leaf_count_sorted(1)._2 > 1) {
            val new_node = new inner_Node(leaf_count_sorted(0)._1 + "^" + leaf_count_sorted(1)._1)
            inner_Node_map += (leaf_count_sorted(0)._1 + "^" + leaf_count_sorted(1)._1 -> new_node)
            inner_expression += leaf_count_sorted(0)._1 + "^" + leaf_count_sorted(1)._1
            leaf_count_sorted(0)._1 + "^" + leaf_count_sorted(1)._1
            new_node.child += leaf_Node_map(leaf_count_sorted(0)._1) 
            new_node.child += leaf_Node_map(leaf_count_sorted(1)._1)
            val curr_node = new root_Node("0")
            for (i <- root) {
                //val curr_node = i
                if (i.expression.contains(leaf_count_sorted(0)._1) & i.expression.contains(leaf_count_sorted(1)._1)) {
                    i.child -= leaf_Node_map(leaf_count_sorted(0)._1)
                    i.child -= leaf_Node_map(leaf_count_sorted(1)._1)
                    val arr = (leaf_count_sorted(0)._1,leaf_count_sorted(1))
                    val idxList: ListBuffer[Int] = ListBuffer[Int]()
                    for(idx  <- 0 until i.child.size) {
                        if(i.child(idx).expression.contains(leaf_count_sorted(0)._1) & i.child(idx).expression.contains(leaf_count_sorted(1)._1)) {
                            idxList += idx
                        }
                    }
                    for (idx <- idxList) {
                        i.child.drop(idx)
                    }


                    //i.child += new_node
                    
                }
                i.child += new_node
                
            }
            curr_node.child += new_node

        } 
       
    }

    private def connect_root_to_second_layer(rootNode: root_Node): Unit = { //把root node連接到second layer node
        
        
        if (rootNode.expression.contains('∨')) {
            rootNode.expression.split('∨').foreach(x => if (x.length() > 1) {
                val new_node = new inner_Node(x)
                global_Node_map += (x -> new_node)
                second_layer_node_map += (x -> new_node)
                rootNode.child += new_node
                second_layer_node_List += new_node
                
            } else {
                val new_node = new leaf_Node(x)
                global_Node_map += (x -> new_node)
                rootNode.child += new_node
            }
            )
        } else {
            val new_node = new inner_Node(rootNode.expression)
            global_Node_map += (rootNode.expression -> new_node)
            rootNode.child += new_node
            second_layer_node_map += (rootNode.expression -> new_node)
            second_layer_node_List += new_node
            
        }
        
    }
    

    private def combine_leaf_as_inner(leaf_count_Map :scala.collection.mutable.Map[String, Int]) : Unit = {

        val leaf_count_kv_pair = leaf_count_Map.toList.sortBy(_._2)(ord = Ordering[Int].reverse)
        

        
        // for(second_layer_node <- second_layer_node_List) {
        //     var will_be_delete_child = ListBuffer[Node]()
        //     for(child <- second_layer_node.child) {
        //         if(child.expression.length() > 1) {
        //             will_be_delete_child += child
        //         }
        //     }
        //     second_layer_node.child.diff(will_be_delete_child)
            
        // }



        for (i <- 0 until leaf_count_kv_pair.size - 1 by 2) {
            val new_combine_expression = leaf_count_kv_pair(i)._1 + "^" + leaf_count_kv_pair(i + 1)._1


            

            for (second_layer_node <- second_layer_node_List) {
                if (second_layer_node.expression.contains(leaf_count_kv_pair(i)._1) & second_layer_node.expression.contains(leaf_count_kv_pair(i + 1)._1)) {
                    val new_inner_node = inner_Node_Map_Set_key.getOrElse(Set(leaf_count_kv_pair(i)._1, leaf_count_kv_pair(i + 1)._1), new inner_Node(new_combine_expression))
                    //inner_Node_map += (new_combine_expression -> new_inner_node)
                    inner_Node_Map_Set_key +=  (Set(leaf_count_kv_pair(i)._1, leaf_count_kv_pair(i + 1)._1) -> new_inner_node)
                    //inner_expression += new_combine_expression
                    if (!second_layer_node.child.contains(inner_Node_Map_Set_key(Set(leaf_count_kv_pair(i)._1, leaf_count_kv_pair(i + 1)._1))) ) {
                        second_layer_node.child += inner_Node_Map_Set_key(Set(leaf_count_kv_pair(i)._1, leaf_count_kv_pair(i + 1)._1))
                    
                        second_layer_node.child -= leaf_Node_map(leaf_count_kv_pair(i)._1)
                        second_layer_node.child -= leaf_Node_map(leaf_count_kv_pair(i + 1)._1)
                        new_inner_node.child += leaf_Node_map(leaf_count_kv_pair(i)._1)
                        new_inner_node.child += leaf_Node_map(leaf_count_kv_pair(i + 1)._1)
                    }
                    second_layer_node.child -= leaf_Node_map(leaf_count_kv_pair(i)._1)
                    second_layer_node.child -= leaf_Node_map(leaf_count_kv_pair(i + 1)._1)

                    

                }
                
            }
            

        }

    }

}



val tree = new ATree
tree.insert("A^B^E^D")
tree.insert("A^B^C^D")
tree.insert("A^E^B^F")
tree.insert("B^D^E")
tree.insert("B^E^F")
tree.insert("C^D^E")
tree.insert("F^E^G")
// tree.insert("B^D^Z^X")
// tree.insert("B^D^Y^K")
// tree.insert("B^D^Y^L")
// println(tree.root(0).expression)
// println(tree.root(1).expression)
// //println(tree.root(0).child.size)
// // println(tree.root(0).child.foreach(x => print(x.expression + ", ")))
// // println(tree.root(1).child.foreach(x => print(x.expression + ", ")))
// // //println(tree.root(2).child.foreach(x => print(x.expression + ", ")))

println(leaf_Node_map.size)
// //val testNode = new inner_Node("B^C")
// tree.root(0).child += testNode
// println(tree.root(0).child.size)


// println(tree.root(0).child.foreach(x => print(x.expression + ", ")))
// println(tree.root(1).child.foreach(x => print(x.expression + ", "))) 
// println(tree.root(2).child.foreach(x => print(x.expression + ", ")))
println(leaf_count_map.toList.sortBy(_._2)(ord = Ordering[Int].reverse))


//global_Node_map.foreach(x => print(x._1 + " -> " + x._2.expression + ", "))
println("\n---------------")
//println(second_layer_node_map.size)
//second_layer_node_map.foreach(x => print(x._1 + " -> " + x._2 + ", "))
tree.root(0).child.foreach(x => print(x.expression + ", "))
println("\n--------------------")


// println(tree.root(1).child(0).expression)
// println(tree.root(1).child(0).child.size)
// println(tree.root(1).child(0).child(0).expression)
// println(tree.root(1).child(0).child(1).expression)
// //println(tree.root(0).child(0).child(0).equals(tree.root(1).child(0).child(0)))
//  println(tree.root(1).child(0).child(2).expression)
//  println(tree.root(1).child(0).child(3).expression)
//println(tree.root(0).child(0).child(0).equals(tree.root(0).child(0).child(1)))

//inner_Node_Map_Set_key.foreach(x => print(x._1 + " -> " + x._2.expression + ", "))
//println(inner_Node_Map_Set_key.size)

//println(inner_Node_Map_Set_key(Set("C", "G")).expression)
//leaf_Node_map.foreach(x => print(x._1 + " -> " + x._2.expression + ", "))