import java.net.Socket
import java.io.BufferedInputStream
import java.io.PrintStream
import java.io.BufferedOutputStream
import scala.io.StdIn._
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Ctest1 extends App{
  //println("Making socket")
  val socket = new Socket("localhost", 5555) 
  //println("Socket made")
  val inputStream = new BufferedReader(new InputStreamReader(socket.getInputStream()))
  val outputStream = new PrintStream(socket.getOutputStream())
  var stopped = false
  
  Future {
    while (!stopped) {
      val p = inputStream.readLine()
      if (p != null) {
        println(p) 
      } else {
        stopped = true
      }
      
    }
  } 
  
  var input = ""
  while (input != ":quit") {
    input = readLine("------\n")
    if (input == "W") {
      whisperMode()
    } else if (input == "G") {
      groupMode()
    } else {
    outputStream.println(input)
    }
  }
  
  inputStream.close()
  outputStream.close()
  
  socket.close()
  readLine("----Press Enter to Stop----")
  println("Program stopped")
  
  def whisperMode(): Unit = {
    var input = ""
    val targetPerson = readLine("Enter person: ")
    println(s"----Send direct message to $targetPerson -----")
    while (input != ":quit") {
      input = readLine()
      if (input != ":quit") outputStream.println("W: " + targetPerson + " " + input)
    }
    println(s"----Stop Direct Mode----")
  }

  def groupMode(): Unit = {
    val peopleList = readLine("Enter people you want to invite: ")
    val invitePeople = peopleList.split(",")
    println(s"start Group Chat with ${invitePeople.mkString(" ")}")
    var input = ""
    while (input != ":quit") {
      input = readLine()
      if (input != ":quit") outputStream.println("G: " + invitePeople.mkString(" ") + "|" + input)   
    }
    println("----Stop Group Mode----")
  }
  
  


}
