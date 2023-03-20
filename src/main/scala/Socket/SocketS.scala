

import java.io._
import java.net._

object SocketS {
  def main(args: Array[String]): Unit = {
    val server = new ServerSocket(9000)
    val clients = scala.collection.mutable.ListBuffer[Socket]()

    while (true) {
      val client = server.accept()
      clients += client
      
      // create a new thread to handle messages from the client
      new Thread(() => {
        
        val itSelf = client
        println(itSelf.toString())
        val inputStream = new BufferedReader(new InputStreamReader(client.getInputStream))
        val outputStreams = clients.filter(_ != itSelf).map(_.getOutputStream)

        while (true) {
          val outputStreams = clients.filter(_ != itSelf).map(_.getOutputStream)
          val message = inputStream.readLine()
          if (message == "null") {
            clients -= client
            client.close()
            false
          }

          outputStreams.foreach(_.write((message + "\n").getBytes("UTF-8")))
        }
      }).start()
    }
  }
}
