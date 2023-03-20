import java.io._
import java.net._


    val server = new ServerSocket(10001)
    val clients = scala.collection.mutable.Set[Socket]()

    while (true) {
      val client = server.accept()
      println("Client connected: " + client.getInetAddress)
      clients += client

      // create a new thread to handle messages from the client
      new Thread(() => {
        println("new thread" + client.getInetAddress)
        val inputStream = new BufferedReader(new InputStreamReader(client.getInputStream))
        val outputStreams = clients.filterNot(_ == client).map(_.getOutputStream)

        while (true) {
          val message = inputStream.readLine()
          if (message == null) {
            clients -= client
            client.close()
            false
          }
          
          outputStreams.foreach(_.write((message + "\n").getBytes("UTF-8")))
        }
      }).start()
    }
  

