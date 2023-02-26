import scala.collection.mutable

// Define a case class to represent a clause in CNF form
case class Clause(literals: Set[String])

// Define a case class to represent a node in the A-Tree
sealed trait Node
case class InnerNode(op: String, left: Node, right: Node) extends Node
case class LeafNode(clause: Clause) extends Node

// Define a case class to represent a reference to a shared subexpression
case class Ref(id: Int)

// Define a case class to represent a query in CNF form
case class Query(clauses: Set[Clause])

// Define a function to compute the hash value of a clause
def hashClause(clause: Clause): Int = clause.literals.hashCode()

// Define a function to build an A-Tree from a set of clauses
def buildTree(clauses: Set[Clause]): Node = {
  val sortedClauses = clauses.toSeq.sortBy(c => c.literals.size)
  val rootNode = build(sortedClauses)
  rootNode
}

// Define a helper function to build an A-Tree from a sequence of clauses
def build(clauses: Seq[Clause]): Node = {
  if (clauses.isEmpty) {
    LeafNode(Clause(Set.empty))
  } else if (clauses.size == 1) {
    LeafNode(clauses.head)
  } else {
    val commonPrefix = findCommonPrefix(clauses)
    if (commonPrefix.isEmpty) {
      val (left, right) = clauses.splitAt(clauses.size / 2)
      val leftNode = build(left)
      val rightNode = build(right)
      InnerNode("&", leftNode, rightNode)
    } else {
      val (prefix, rest) = clauses.partition(_.literals == commonPrefix)
      val innerNode = InnerNode("&", LeafNode(Clause(commonPrefix)), build(rest))
      if (prefix.size == 1) {
        innerNode
      } else {
        val refId = nextRefId()
        refs.put(refId, innerNode)
        Ref(refId)
      }
    }
  }
}

// Define a helper function to find the common prefix of a sequence of clauses
def findCommonPrefix(clauses: Seq[Clause]): Set[String] = {
  val headLiterals = clauses.head.literals
  var prefix = headLiterals
  for (clause <- clauses.tail) {
    prefix = prefix.intersect(clause.literals)
    if (prefix.isEmpty) {
      return Set.empty
    }
  }
  prefix
}

// Define a global mutable hash table to store references to shared subexpressions
val refs = mutable.HashMap.empty[Int, Node]
var refId = 0
def nextRefId(): Int = {
  refId += 1
  refId
}

// Define a function to optimize a set of queries by factoring out common subexpressions
def optimize(queries: Set[Query]): Set[Query] = {
  val uniqueClauses = mutable.HashMap.empty[Clause, Int]
  val newQueries = mutable.Set.empty[Query]
  for (query <- queries) {
    val clauses = query.clauses.map(c => {
      if (uniqueClauses.contains(c)) {
        Ref(uniqueClauses(c))
      } else {
        val clauseId = nextRefId()
        uniqueClauses.put(c, clauseId)
        buildTree(Set(c))
        LeafNode(c)
      }
    })
    newQueries.add(Query(clauses))
  }
  newQueries.toSet
}

val query1 = Set("(A OR B) AND (C OR D)")
val query2 = Set("(A OR B) AND (E OR F)")
val root = buildTree(query1)
println(root)