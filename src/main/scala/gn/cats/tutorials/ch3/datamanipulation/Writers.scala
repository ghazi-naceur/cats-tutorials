package gn.cats.tutorials.ch3.datamanipulation

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

// 2
object Writers {

  import cats.data.Writer
  val aWriter: Writer[List[String], Int] = Writer(List("Isaac Netero"), 125)
  // List[String] considered as 'logs' type
  // Int considered as a 'value' type
  // Writer[List[String], Int] can be considered as an 'Option[Int]' with additional info (List[String])
  val anIncreaseWriter: Writer[List[String], Int] = aWriter.map(_ + 1) // value = 126, logs stay the same
  val aLogsWriter: Writer[List[String], Int] = aWriter.mapWritten(_ :+ " is an old man")
  // value stays the same, logs gets another string

  // Modifying value and logs at the same:
  // 1- with bimap:
  val aWriterWithBiMap: Writer[List[String], Int] = aWriter.bimap(_ :+ " is an old man", _ + 1)
  // 2- with mapboth
  val aWriterWithMapBoth: Writer[List[String], Int] = aWriter.mapBoth { (logs, value) =>
    (logs :+ " is an old man", value + 1)
  }

  /*
    The point of Writers is to define at the start and manipulate them with pure FP, and finally dump either the value
    or the logs
   */
  val desiredValue: Int = aWriter.value // right side: Int
  val logs: List[String] = aWriter.written // left side: List[String]
  val (logs2, desiredValue2) = aWriter.run // extracting both using run

  import cats.instances.vector._ // imports a Semigroup[Vector]
  val writerA: Writer[Vector[String], Int] = Writer(Vector("Log 1", "Log 2"), 10)
  val writerB: Writer[Vector[String], Int] = Writer(Vector("Log 3"), 40)
  val compositeWriter: Writer[Vector[String], Int] = for {
    va <- writerA
    vb <- writerB
  } yield va + vb
  // The left side will be combined naturally in the presence of a Semigroup which will define a combination, in our case
  // it's a combination function of Vector, so import cats.instances.vector._

  // reset the logs and keep the desired value
  // Reseting meaning that we'll have an empty list, so we need to import a Monoid that will define that 'empty' value:
  import cats.instances.list._
  val amEmptyWriter: Writer[List[String], Int] = aWriter.reset

  // Rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting"), 0)
    else
      countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n)) // 'bimap' and 'mapBoth' instead of 'flatMap' work also
  }

  // Rewrite this method with Writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithLogs(n - 1)
        _ <- Writer(Vector(s"Computer sum(${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n
  }

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    println(amEmptyWriter.run)
    println(countAndSay(5))
    println(countAndLog(5).written.foreach(println))
    println(naiveSum(5))
    println(sumWithLogs(5).written.foreach(println))
    /*
      If we're using Future for 'naiveSum', the logs won't be ordered and this will create confusion. But with Writers
      we can separate logs and know which logs from which thread
     */
    println("Using Future :")
    Future(naiveSum(5)).foreach(println)
    Future(naiveSum(5)).foreach(println)

    val future1 = Future(sumWithLogs(5))
    val future2 = Future(sumWithLogs(5))
    val logs1 = future1.map(_.written)
    val logs2 = future2.map(_.written)
    logs1.foreach(println)
    logs2.foreach(println)
  }
}
