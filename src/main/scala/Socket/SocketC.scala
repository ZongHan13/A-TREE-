

import java.io._
import java.net._
import java.util.Scanner
object SocketC {
  def main(args: Array[String]): Unit = {
    val client = new Socket("localhost", 9000)
    val inputStream = new BufferedReader(new InputStreamReader(client.getInputStream))
    val outputStream = new PrintWriter(client.getOutputStream, true)

    // create a new thread to handle incoming messages from the server
    new Thread(() => {
      while (true) {
        val message = inputStream.readLine()
        if (message == null) {
          client.close()
          return
        }

        println(message)
      }
    }).start()

    // read messages from the console and send them to the server
    val scanner = new Scanner(System.in)
    while (scanner.hasNextLine) {
      val message = scanner.nextLine()
      outputStream.println(message)
    }
  }
}

