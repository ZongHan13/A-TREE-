import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
class ReaderWriter {
  val lock = new ReentrantReadWriteLock()
  val data = Array.fill(20)(0)

  def read(): List[Int] = {
    val readLock = lock.readLock()
    readLock.lock()
    try {
      data.toList
    } finally {
      readLock.unlock()
    }
  }

  def write(value: Int): Unit = {
    val writeLock = lock.writeLock()
    writeLock.lock()
    try {
      (0.until(data.length)).foreach(i => data(i) = value)
    } finally {
      writeLock.unlock()
    }
  }
}

object ReadWriteTest extends App {
  val rw = new ReaderWriter()

// 启动多个线程并发地读取数据

// 启动多个线程并发地写入数据
  val b = new Thread(() =>
    (1 to 30).foreach { i =>
      new Thread(() => {
        rw.write(i)
        println(s"Writer $i wrote data")
        //Thread.sleep(100)
      }).start()
    }
  )
  Thread.sleep(100)
  val a = new Thread(() =>
    
    (1 to 10).foreach { i =>
      new Thread(() => {
        println(s"Reader $i: ${rw.read()}")

      }).start()
    }
  )
  b.start()
  a.start()
  // b.join()
  // a.join()  
  //Thread.sleep(2000)

  //println("Final read : " + rw.read())

}
