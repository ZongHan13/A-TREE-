import akka.actor._

//this version will have race condition because of the shared data is not Actor


case object Read
case class Write(data: Int)

class Reader extends Actor {
  def receive = {
    case Read =>
      println(s"Reader ${self.path.name} read data: ${SharedData.read.mkString(", ")}")
  }
}

class Writer extends Actor {
  def receive = {
    case Write(data) =>
      println(s"Writer ${self.path.name} wrote data: $data")
      SharedData.write(data)
      
  }
}

object SharedData {
  val data: Array[Int] = Array.fill(50)(0)

  def read: Array[Int] = {
    data
  }

  def write(newData: Int): Unit = {
    0.until(data.length).foreach(i => data(i) = newData)
  }
}


object ReadWriteProbAkkaVer extends  App{
  val system = ActorSystem("ReadWriteSystem")

  val reader1 = system.actorOf(Props[Reader], name = "reader1")
  val reader2 = system.actorOf(Props[Reader], name = "reader2")
  val reader3 = system.actorOf(Props[Reader], name = "reader3")
  val reader4 = system.actorOf(Props[Reader], name = "reader4")
  val reader5 = system.actorOf(Props[Reader], name = "reader5")
  val writer1 = system.actorOf(Props[Writer], name = "writer1")
  val writer2 = system.actorOf(Props[Writer], name = "writer2")
  val writer3 = system.actorOf(Props[Writer], name = "writer3")
  val writer4 = system.actorOf(Props[Writer], name = "writer4")
  val writer5 = system.actorOf(Props[Writer], name = "writer5")
  val writer6 = system.actorOf(Props[Writer], name = "writer6")
  // Reader-Writer problem: readers-preference
  reader1 ! Read
  reader2 ! Read
  writer1 ! Write(1)
  reader3 ! Read
  reader4 ! Read
  reader5 ! Read
  writer3 ! Write(3)
  writer4 ! Write(4)
  writer5 ! Write(10)
  writer6 ! Write(6)
  reader1 ! Read
  reader2 ! Read
  writer2 ! Write(2)
  reader1 ! Read
  reader2 ! Read
  Thread.sleep(2000)
  writer1 ! Write(33)
  reader5 ! Read

  system.terminate()
}
