import scalaj.http._

object FirstHttp extends App{
  //val testPost = Http("https://maker.ifttt.com/trigger/{scala_event}/json/with/key/cyMr3y7V3Np-gzMAhWE8HM").postData("Hello").asString
  val testRequest = Http("https://maker.ifttt.com/trigger/scala_event/json/with/key/cyMr3y7V3Np-gzMAhWE8HM").asString
  println(testRequest)




}