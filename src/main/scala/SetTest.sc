import scala.util.control.Breaks._

val Set1 = Set("A","B","E")

val list1 = List(Set("A", "B", "E", "F"), Set("C", "D", "G", "Z"))//List(Set("A","B","C"), Set("A", "B", "E", "F"), Set("A", "B", "D", "E"))
def reorganize(_expression: String):Set[Set[String]] = {
        var u = _expression.replace("^","").toSet
        var c : Set[Set[String]] = Set.empty
        breakable{
        while (u.nonEmpty) {
            val s = find_max_intersect(charSetToStringSet(u),list1)
            println("s: " + s)
            if( s.isEmpty ) {
                break()
            }
            u = u -- stringSetToCharSet(s)
            println("u: " + u)
            c = c.union(Set(s))
            println("c: " + c)
            }
        }   
        
        
        c.union(Set(charSetToStringSet(u)))

    }



def find_max_intersect(set1:Set[String], target: List[Set[String]]): Set[String] = {
        var maxinterSet: Set[String] = Set.empty
        for(i <- target) {
            val interSet = set1.intersect(i)
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
    

println("RESULT" + reorganize("A^B^C^D"))