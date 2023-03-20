import akka.actor._

// this version will not have any race condition

case object Read1
case class Write1(data: Int)

class Reader1(sharedData: ActorRef) extends Actor {
  def receive = {
    case Read1 =>
      sharedData ! Read1
  }
}

class Writer1(sharedData: ActorRef) extends Actor {
  def receive = {
    case Write1(data) =>
      sharedData ! Write1(data)
  }
}

class SharedData1 extends Actor {
  var data: Array[Int] = Array.fill(50)(0)

  def receive = {
    case Read1 =>
      println(s"Reader ${sender().path.name} read data: ${data.mkString(", ")}")
    case Write1(newData) =>
      0.until(data.length).foreach(i => data(i) = newData)
      println(s"Writer ${sender().path.name} wrote data: $newData")
  }
}

object ReadWriteProbAkkaVerFix extends  App{
  val system = ActorSystem("ReadWriteSystem")

  val sharedData = system.actorOf(Props[SharedData1], name = "sharedData")
  val reader1 = system.actorOf(Props(new Reader1(sharedData)), name = "reader1")
  val reader2 = system.actorOf(Props(new Reader1(sharedData)), name = "reader2")
  val reader3 = system.actorOf(Props(new Reader1(sharedData)), name = "reader3")
  val reader4 = system.actorOf(Props(new Reader1(sharedData)), name = "reader4")
  val reader5 = system.actorOf(Props(new Reader1(sharedData)), name = "reader5")
  val writer1 = system.actorOf(Props(new Writer1(sharedData)), name = "writer1")
  val writer2 = system.actorOf(Props(new Writer1(sharedData)), name = "writer2")
  val writer3 = system.actorOf(Props(new Writer1(sharedData)), name = "writer3")
  val writer4 = system.actorOf(Props(new Writer1(sharedData)), name = "writer4")
  val writer5 = system.actorOf(Props(new Writer1(sharedData)), name = "writer5")
  val writer6 = system.actorOf(Props(new Writer1(sharedData)), name = "writer6")

  // Reader-Writer problem: readers-preference
  reader1 ! Read1
  reader2 ! Read1
  writer1 ! Write1(1)
  reader3 ! Read1
  reader4 ! Read1
  reader5 ! Read1
  writer3 ! Write1(3)
  writer4 ! Write1(4)
  writer5 ! Write1(10)
  writer6 ! Write1(6)
  reader1 ! Read1
  reader2 ! Read1
  writer2 ! Write1(2)
  reader1 ! Read1
  reader2 ! Read1
  Thread.sleep(2000)
  writer1 ! Write1(33)
  reader5 ! Read1

  system.terminate()
}
