package gn.cats.tutorials.ch4.typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

// 7
object Traversing {

  implicit val ex: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(6))

  val servers: List[String] = List("server1", "server2", "server3")

  def getBandWidth(hostname: String): Future[Int] = Future(hostname.length * 50)

  /*
   We have:
    - a List[String]
    - a func String => Future[Int]
   We want a Future[List[Int]]
   */

  // Manual solution:
  val allBandWidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture: Future[Int] = getBandWidth(hostname)
    for {
      accBandWidth <- accumulator
      band <- bandFuture
    } yield accBandWidth :+ band
  }

  // "Automatic" solution: traverse
  val allBandWidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandWidth)
  val allBandWidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandWidth))
  // List[Future] (servers.map(getBandWidth)) + sequence = Future[List]

  // Exercise
  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  def listTraverse[F[_]: Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      val wElement: F[B] = func(element)
      for {
        acc <- wAccumulator
        elem <- wElement
      } yield acc :+ elem
    }

  import cats.syntax.apply._ // mapN
  def listTraverse2[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      val wElement: F[B] = func(element)
      (wAccumulator, wElement).mapN(_ :+ _)
    }
  // Applicative doesn't have access to map and flatMap, but I can still combine the wrapper of an accumulator with a
  // wrapper of an element, with another function: 'mapN'

  // Exercise:
  def listSequence[F[_]: Applicative, A](list: List[F[A]])(implicit monad: Monad[F]): F[List[A]] =
    listTraverse(list)(identity) // (identity) ==  (x => x)

  // Exercise:
  import cats.instances.vector._
  val allPairs: Vector[List[Int]] = listSequence(List(Vector(1, 2, 3), Vector(4, 5))) // all 2-pairs
  val allTriples: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) // all 3-pairs

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
  }
}
