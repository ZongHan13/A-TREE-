import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
// import Behavior




import java.net._
import java.io._
import net.sourceforge.argparse4j.inf.Argument

object SimpleActorExample {
  val array1 = Array("Bob","Helen","Ger","Frank","Hanna","John")
   def matchNumber(str: String): Boolean = {
      val pattern = "[0-9]+".r
      pattern.matches(str)
      
    }
  def matchFloat(str: String): Boolean = {
    //match 1.9 .900 23.14
    val pattern = "[+-]?([0-9]*[.])?[0-9]+".r
    pattern.matches(str)
  }


  def main(args: Array[String]): Unit = {
    def sendWithIndex(int: Int):Unit = {
      if (int < array1.size) {
      val forSend =  array1.slice(int, array1.size)
      forSend.foreach(i => println("Send notification to: " + i))
      }
      println()
    }
    class SimpleActor() extends Actor {
      import context._
      //val myactor = actorOf(Props[SimpleActor], "myActor")
      def receive: Receive = {
        //case s: String => println("String" + s)
        case i: Int => sendWithIndex(i)
        case f: Float => println("This is Float: " + f)
        case _ => println("Unknown type")
        // case s: String => myactor ! "Hello" ; println( myactor.path + s)
        //case s : String => println("String " + s + " " +  self.path + " Sender: " + sender().path)
        
      }
      

      def foo = println("Normal method")
      

    }
    

    val system = ActorSystem("SimpleSystem")
    val actor = system.actorOf(Props[SimpleActor], "SimpleActor")
    val actor2 = system.actorOf(Props[SimpleActor], "SimpleActor2")
    //val actor1 = system.actorOf(Props[SimpleActor], "SimpleActor1")

    







    val serverSocket = new ServerSocket(9999)
    val socket = serverSocket.accept()
    val inputStream = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    val outputStream = new PrintStream(socket.getOutputStream())
    //println(actor.path)
    while (true) {
      val message = inputStream.readLine()
      message match {
        case number if(matchNumber(number)) => actor ! number.toInt 
        case float if (matchFloat(float)) => actor! float.toFloat
        case _ => actor ! message
      }
      
    }
    
    
  }
}
