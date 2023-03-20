import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props



object SimpleActorExample {
  def main(args: Array[String]): Unit = {

    class SimpleActor extends Actor {
      def receive: Receive = {
        case s: String => println("String" + s)
        case i: Int => println("Number:" + i)
        case f: Float => println("Float: " + (f + 10.5.toInt))
        case _ => foo 
      }

      def foo = println("Normal method")

    }

    val system = ActorSystem("SimpleSystem")
    val actor = system.actorOf(Props[SimpleActor], "SimpleActor")

    println("Before messages")
    actor ! "Hi there."
    println("After string")
    actor ! 42
    println("After int")
    actor ! 'a'
    println("After char")
    actor ! 3.5.toFloat
    println("After Float")
  }
}
