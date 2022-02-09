package gn.cats.tutorials.ch4.typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

// 1
object Semigroupals {

  // Semigroupal a Higher-kinded type class which can tuple elements

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]
  val aTupledOption: Option[(Int, String)] =
    optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled: Option[(Int, Nothing)] = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._ // implicit Semigroupal[Future]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(6))
  val aTupledFuture: Future[(String, Int)] =
    Semigroupal[Future].product(Future("Something"), Future(12)) // Future(("Something", 12))

  import cats.instances.list._ // Monad[List] == Semigroupal[List]
  val aTupledList: List[(Int, String)] = Semigroupal[List].product(List(1, 2), List("a", "b"))
  // We will obtain a cartesian product, because the Monad[List] will run the 'product' in terms of for-comprehension
  // and this is why we obtain the cartesian product.

  // Exercise:
  import cats.Monad
  import cats.syntax.functor._ // map
  import cats.syntax.flatMap._ // flatMap
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
//    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))
//  or simply:
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  trait MyMonad[M[_]] extends Semigroupal[M] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] = flatMap(fa)(a => map(fb)(b => (a, b)))
  }

  // Monads extend Semigroupals.
  // Monad product follows a for-comprehension, which is a sequence of maps and flatmaps, which will obey the Monad laws.
  // The Monad laws impose some sequencing of operations. Whereas, we may want to combine values without necessarily
  // impose a sequencing of evaluations. Validated is a use case for Semigroupal, because it combines instances of type
  // Validated without needing to follow the Monad laws.
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal: Semigroupal[ErrorsOr] = Semigroupal[ErrorsOr]
  // 'Semigroupal[ErrorsOr]' will implement a 'product' of 2 Validated instances in terms of independently combining the
  // error type and the value type T, according to their Semigroup. So this requires the implicit Semigroup[List[_]]
  // (import cats.instances.list._)
  val invalidCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else is wrong")),
    Validated.invalid(List("This can't be right"))
  )
  // => These errors will combined in 1 list: Invalid(List(Something wrong, Something else is wrong, This can't be right))

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either] // obeying the Monad laws ==> sequencing
  val eitherSemigroupal: Semigroupal[EitherErrorsOr] = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // implemented in terms of map and flatMap
    Left(List("Something wrong", "Something else is wrong")),
    Left(List("This can't be right"))
  )
  // => Result: Left(List(Something wrong, Something else is wrong)) // First list only: Left part, because the flatMap
  // on Either short-circuits the evaluation of the second Either (Left(List("This can't be right"))).
  // Using Either in this case, is not useful because we loose track of errors. In the hand, Validated is more efficient
  // in this case.

  // Associativity law : m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
  // This law is true for Either, but it's not for Validated

  // Exercise: Define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(aTupledList) // Cartesian product : List((1,a), (1,b), (2,a), (2,b)) / All possible combination cases
    println(invalidCombination)
    println(eitherCombination)
    println(zipListSemigroupal.product(List(1, 2), List("a", "b")))
  }
}
