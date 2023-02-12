import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.spark.sql.DataFrame
import scala.reflect.io.File
import myFile._


object KafkaTest {
  val spark = SparkSession.builder().master("local[*]").appName("KafkaTest").getOrCreate()
  spark.sparkContext.setLogLevel("ERROR") 
  val fw = myFile
  def main(args:Array[String]): Unit = {
  //fw.writeFirst
  import spark.implicits._

  val df = spark.readStream.format("kafka").option("kafka.bootstrap.servers","134.208.2.169:9092").option("subscribe", "SparkTest").load()

  val df1 =  df.selectExpr("CAST(key AS STRING)", "CAST(value AS STRING)")
  df1.printSchema()
  //df1.writeStream.format("console").outputMode("append").start().awaitTermination()
  //val wwww = df1.where("value = Hello")
  //wwww.writeStream.format("")
  //if (df1.select("value").as("String").equals("Hello")) {println("Nice to meet you")} 
  //df1.writeStream.format("console").start()
  //val df2 = df1.writeStream.foreach(x => if(col("value").equals("Hello")){ println("Got!")} )
  //val df2 = df1.select("value").foreach(x => if(x.toString().equals("Hello")) { println("Got!")})
  // val query = df1.writeStream.foreachBatch((data:DataFrame, id:Long)  => if(data.isEmpty) {} 
  // else {data.foreach(x=> if(x(1).equals("Hello")) {fw.writeHello})//fw.writeappend}        )
  
  // }     
  // ).start()

  val query = df1.writeStream.foreachBatch((data: DataFrame, id: Long) => if(!data.isEmpty) { data.foreach(x => x(1).toString match {
    case "Hello" => fw.writeHello
    case s:String => fw.writeText(s)
    
  }   )
    }
  ).start()

  //df1.writeStream.start().awaitTermination()
  //spark.streams.awaitAnyTermination()
  query.awaitTermination()

  }
}
