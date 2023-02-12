
import java.io._
import java.sql.Time
import java.util.Locale
import org.apache.spark.sql.catalyst.util.DateTimeUtils
import java.util.Calendar
import java.time.LocalDateTime
import java.time.LocalDate
import java.time.LocalTime
import java.time.format.DateTimeFormatter

object myFile {
  val now = Calendar.getInstance()
  
  def writeHello = {
  
  // val currentHour = now.get(Calendar.HOUR)
  // val currentMiunte = now.get(Calendar.MINUTE)
  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  val localDateTime = LocalDateTime.now()
  val formatterTime = localDateTime.format(formatter)
  os.write.append(os.pwd/"test.txt", "Hello " + formatterTime  +  "\n")
  
  
}

  def writeText(text: String) = {
  
  // val currentHour = now.get(Calendar.HOUR)
  // val currentMiunte = now.get(Calendar.MINUTE)
  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  val localDateTime = LocalDateTime.now()
  val formatterTime = localDateTime.format(formatter)


  os.write.append(os.pwd/"test.txt", text + " " + formatterTime +  "\n")
    
  }
  


  

}