import java.io.{DataInputStream, DataOutputStream, ObjectOutputStream}
import java.net.{InetAddress, Socket}


// Create a socket to connect to the server
//val ia: InetAddress = InetAddress.getByName("localhost")
//val socket: Socket = new Socket(ia, 9999)
// Create an input, output stream to receive data from the server
// val in: DataInputStream = new DataInputStream(socket.getInputStream)
// val out: ObjectOutputStream = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream))

while (true) {
    val ia: InetAddress = InetAddress.getByName("localhost")
    val socket: Socket = new Socket(ia, 9999)
    val forSend = scala.io.StdIn.readLine("Enter your Message:")
    val in: DataInputStream = new DataInputStream(socket.getInputStream)
    val out: ObjectOutputStream = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream))
    if (forSend == "exit") {
        out.close()
        in.close()
        socket.close()
        System.exit(0)
    }
    //
    println("Sending to server: " + forSend)
    out.writeObject(forSend.toString())
    //out.flush()
    // out.close()
    // in.close()
    val result = in.readUTF()
    out.close()
    in.close()
    println("Received from server: " + result)
    //
    socket.close()
}