package gn.cats.tutorials.ch4.typeclasses

import cats.{Applicative, Foldable, Functor, Monad}

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
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse2(list)(identity) // (identity) ==  (x => x)

  // Exercise:
  import cats.instances.vector._
  val allPairs: Vector[List[Int]] = listSequence(List(Vector(1, 2, 3), Vector(4, 5))) // all 2-pairs
  val allTriples: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) // all 3-pairs

  // Exercise:
  import cats.instances.option._
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse2[Option, Int, Int](list)(n => Some(n).filter(predicate))

  val allTrue: Option[List[Int]] = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2,4,6))
  val someFalse: Option[List[Int]] = filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  // Exercise:
  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List] => to generate Applicative[ErrorsOr]
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse2[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"Predicate for $n failed"))
    }

  val allTrueValidated: ErrorsOr[List[Int]] = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2,4,6))
  val someFalseValidated: ErrorsOr[List[Int]] = filterAsValidated(List(1, 2, 3))(_ % 2 == 0)
  // Invalid(List("Predicate for 1 failed", "Predicate for 3 failed"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    import cats.Id // will help to create an implicit Applicative automatically to be injected in the scope
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]
  val allBandwidthsCats: Future[List[Int]] = Traverse[List].traverse(servers)(getBandWidth)

  // Extension methods
  import cats.syntax.traverse._ // sequence + traverse
  val allBandwidthCtas2: Future[List[Int]] = servers.traverse(getBandWidth)

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)
  }
}
