import java.net.ServerSocket
import java.io.BufferedInputStream
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.InputStreamReader
import java.io.BufferedReader
import java.io.InputStream
import java.net.Socket
import collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import java.util.concurrent.ConcurrentHashMap
import scala.io.AnsiColor._


 
object Stest1 extends App{
  case class User(name: String, sock: Socket, inputStream: BufferedReader, outputStream: PrintStream)
  val users = new ConcurrentHashMap[String, User]().asScala
  Future{ checkConnections() }
  while (true) {
    for((name, user) <- users) {
      doChat(user)
    }
    Thread.sleep(100)
  }


  def checkConnections(): Unit = {
    val ss = new ServerSocket(5555)
    while (true) {
    val sock = ss.accept()//val inputStream = new BufferedInputStream(sock.getInputStream())
    val inputStream = new BufferedReader(new InputStreamReader(sock.getInputStream()))
    val outputStream = new PrintStream(sock.getOutputStream())
    Future {
      outputStream.println("What is your name ?")
      val name = inputStream.readLine()
      val user = User(name, sock, inputStream, outputStream)
      users += name -> user
      }
    }
  }

  def nonBlockingRead(in: BufferedReader): Option[String] = {
    if(in.ready()) Some(in.readLine()) else None
  }



  def doChat(user:User): Unit = {
    nonBlockingRead(user.inputStream).foreach{ input =>
      if(input == ":quit") {
        user.sock.close()
        users -= user.name
        for((name, u) <- users) {
          u.outputStream.println(s"${RED + user.name + " " + "is leave." + RESET}")
        }

      } else if(input.contains("W:")) {
        val targetPerson = input.split(" ")(1)
        if(users.contains(targetPerson)) {
          users(targetPerson).outputStream.println(s"${GREEN + user.name +  " whisper to you: "+RESET + CYAN + input.split(" ").drop(2).mkString(" ")+ RESET}")
        } else {
          user.outputStream.println(s"${RED + "The User is not online" + RESET}")
        }
      } else if (input.contains("G:")) {
        val targetPerson = input.split('|')(0).substring(3).split(" ")
        for(target <- targetPerson) {
          users(target).outputStream.println(s"${GREEN + "Group chat|" + user.name + " says: " + input.split('|')(1) + RESET}")
        }
      } else if (input == "list"){
        val useList = users.keys.toArray.sortBy(_.charAt(0))
        user.outputStream.println("User List: \n" + useList.mkString("\n"))
      } else {
        for((name, u ) <- users) {
          u.outputStream.println(user.name + " : " + input)
        }
      }
    }
  }

}
