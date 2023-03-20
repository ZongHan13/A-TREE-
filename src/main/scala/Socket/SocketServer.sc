import java.net.ServerSocket
import java.io.{DataInputStream, DataOutputStream, ObjectInputStream}
import java.net.{InetAddress, Socket}

val listener: ServerSocket = new ServerSocket(9999)
println("The server is running...")


def addTen(x: Int): Int = {
        x + 10
    }
while (true) {
    val socket: Socket = listener.accept()
    val out = new DataOutputStream(socket.getOutputStream)
    val in = new ObjectInputStream (new DataInputStream(socket.getInputStream))
    
    val read = in.readObject()
    printf("Received from client: %s", read + "\n")

    val result = read.toString() match {
        case "1" => "Hello"
        case "1+1" => "2"
        case "Hi" => "Hello"
        case x if (x.contains("addTen")) => addTen(x.split("addTen").apply(1).toInt)
        case _ => "I don't know"

    }
    out.writeUTF(result.toString())
    out.flush()
    
    //out.close()
    //in.close()
    //socket.close()

    


}