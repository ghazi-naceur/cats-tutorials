package gn.cats.tutorials.ch2.abstractmath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

// 7
object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit Monad[List]
  import cats.instances.future._

  // Option transformer
  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2), Option(3)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option('c'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // Either transformer
  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("Some error"), Right(12), Right(10)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(6))
//  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future[Either[String, Int]](Right(12)))
  // We added the type explicitly in 'EitherT(Future[Either[String, Int]](Right(12)))' to fix a compilation error related
  // to variance. Or simply we can write:
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(12))
  // == EitherT wrap over Future(Right(12))

  /*
    TODO:
      We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
      We measure bandwidth in units
      We want to allocate 2 of our servers to cope with the traffic spike
      We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250
   */

  val bandwidths = Map("server1" -> 50, "server2" -> 300, "server3" -> 170)

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
//    case None        => EitherT(Future[Either[String, Int]](Left(s"Server $server unreachable")))
//    case Some(value) => EitherT(Future[Either[String, Int]](Right(value)))
    case None        => EitherT.left(Future(s"Server $server unreachable"))
    case Some(value) => EitherT.right(Future(value))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1)
    band2 <- getBandwidth(s2)
  } yield band1 + band2 > 250
  // => End type: Future[Either[String, Boolean]]

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers $s1 and $s2 cannot cope with the incoming spike: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 cannot cope with the incoming spike: Not enough total bandwidth")
      case Right(true)  => Right(s"Servers $s1 and $s2 can cope with the incoming spike")
    }
  // => End type: Future[Either[String, String]]
  // 'transform' transforms the Either[String, Boolean] to Either[String, String]

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    val resultFuture1 = generateTrafficSpikeReport("server1", "server3").value
    resultFuture1.foreach(println)
    val resultFuture2 = generateTrafficSpikeReport("server1", "server2").value
    resultFuture2.foreach(println)
    val resultFuture3 = generateTrafficSpikeReport("server2", "server3").value
    resultFuture3.foreach(println)
  }

}
