import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

class ActorReplyExample extends Actor{  
   val child = context.actorOf(Props[ActorChildReplyExample],"ActorChild");  
  def receive = {  
    case s: String if (s.contains("I got you message")) => {println("Message recieved from "+sender.path.name+" massage: "+s + " //:" + self.path) 
    child ! "OK got it!" }
    case s: String if (s.contains("Hello")) => {println("Message recieved from "+sender.path.name+" massage: "+s + " //:" + self.path)
    child ! "Hello"  
  }
    case message:String => {println("Message recieved from "+sender.path.name+" massage: "+message+ " //:" + self.path)  
    //child ! "Hello"    
  }
    //val child = context.actorOf(Props[ActorChildReplyExample],"ActorChild");  
    
  }  
}  

class ActorChildReplyExample extends Actor{  
  def receive ={  
    case s :String if (s.contains("Hello")) => {println("Message recieved from "+sender.path.name+" massage: "+s+ " //:" + self.path)
    sender()! "I got you message"
    println("Replying to "+sender().path.name);   
  }
    case message:String => {println("Message recieved from "+sender.path.name+" massage: "+message+ " //:" + self.path)
    println("Replying to "+sender().path.name); 
  }

    //println("Replying to "+sender().path.name);  
    //sender()! "I got you message";  
  }  
}  




object ActorReplyExample{
  
  def main(args:Array[String]){  
    val actorSystem = ActorSystem("ActorSystem");  
    val actor = actorSystem.actorOf(Props[ActorReplyExample], "RootActor");  
    actor ! "Hello";  
  }  

}
