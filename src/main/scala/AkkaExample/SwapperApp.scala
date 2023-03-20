import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.event.Logging

case object Swap
class Swapper extends Actor {
  import context._
  val log = Logging(system, this)

  def receive = {
    case Swap =>
      //log.info("Hi")
      println("Hi")
      become({
        case Swap =>
          println("Ho")
          //log.info("Ho")
          unbecome() // resets the latest 'become' (just for fun)
      }, discardOld = false) // push on top instead of replace
  }

}





object SwapperApp extends App{

  val system = ActorSystem("SwapperSystem")
  val swap = system.actorOf(Props[Swapper], "Swapper")
  swap ! Swap
  swap ! Swap
  swap ! Swap
  swap ! Swap
  swap ! Swap
  swap ! Swap

  
}
