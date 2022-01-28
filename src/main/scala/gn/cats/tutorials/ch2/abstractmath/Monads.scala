package gn.cats.tutorials.ch2.abstractmath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

// 4
object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1: How do you create all combinations of (number, char) ?
  val combinations1: List[(Int, Char)] = numbersList.flatMap(num => charsList.map(ch => (num, ch)))
  val combinations2: List[(Int, Char)] = for {
    num <- numbersList
    ch <- charsList
  } yield (num, ch)

  // option
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('b')

  // TODO 1.2: How do you create all combinations of (number, char) ?
  val combinations3: Option[(Int, Char)] = numberOption.flatMap(num => charOption.map(ch => (num, ch)))
  val combinations4: Option[(Int, Char)] = for {
    num <- numberOption
    ch <- charOption
  } yield (num, ch)

  // future
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(6))
  val numberFuture: Future[Int] = Future(3)
  val charFuture: Future[Char] = Future('c')

  // TODO 1.2: How do you create all combinations of (number, char) ?
  val combinations5: Future[(Int, Char)] = numberFuture.flatMap(num => charFuture.map(ch => (num, ch)))
  val combinations6: Future[(Int, Char)] = for {
    num <- numberFuture
    ch <- charFuture
  } yield (num, ch)

  /*
    Monad Pattern:
      - wrapping a value into a M value
      - the flatmap mechanism
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad: Monad[Option] = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption: Option[Int] = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._ // implicit Monad[List]
  val listMonad: Monad[List] = Monad[List]
  val aList: List[Int] = listMonad.pure(3)
  val aTransformedList: List[Int] = listMonad.flatMap(aList)(x => List(x, x + 1))

  import cats.instances.future._
  val futureMonad: Monad[Future] = Monad[Future] // requires an implicit ExecutionContext
  val aFuture: Future[Int] = futureMonad.pure(5)
  val aTransformedFuture: Future[Int] = futureMonad.flatMap(aFuture)(x => Future(x + 1))

  // Specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(num => chars.map(ch => (num, ch)))
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
    number.flatMap(num => char.map(ch => (num, ch)))
  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] =
    number.flatMap(num => char.map(ch => (num, ch)))

  // Generalized API
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  def main(args: Array[String]): Unit = {
    println(combinations1)
    println(combinations2)
    // The 2 implementations are IDENTICAL

    println(combinations3)
    println(combinations4)
    // The 2 implementations are IDENTICAL

    println(combinations5)
    println(combinations6)
    // The 2 implementations are IDENTICAL

    println(aTransformedOption) // None
    println(aTransformedList) // List(3, 4)
    println(aTransformedFuture) // Future(Success(6))

    // Generalized API
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    getPairs(numberFuture, charFuture).foreach(println)
  }
}
