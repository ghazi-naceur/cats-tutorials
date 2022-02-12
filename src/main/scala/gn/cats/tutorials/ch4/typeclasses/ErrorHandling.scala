package gn.cats.tutorials.ch4.typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

// 5
object ErrorHandling {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    // pure method from Applicative
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
  val success: ErrorOr[Int] = monadErrorEither.pure(12) // Either[String, Int] == Right(12)
  val failure: ErrorOr[Int] =
    monadErrorEither.raiseError[Int]("Something wrong") // Either[String, Int] == Left("Something wrong")

  // Equivalent of "recover", which takes a function from an error type to a value type
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _         => 23
  }

  // Equivalent of "recoverWith", which takes a function from an error type to another wrapper type, not another value type
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _         => Left("Something else") // ErrorOr[Int]
  }

  // "filter", which turns a value to an error type
  val filteredSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("Number too small")(_ > 10)

  // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try], E = Throwable
  val exception = new RuntimeException("Really bad")
  val pureException: Try[Nothing] = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)
  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(6))
  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)

  // Applicative => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List], this will create an ApplicativeError[ErrorsOr, List[String]]
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applErrorVal: ApplicativeError[ErrorsOr, List[String]] = ApplicativeError[ErrorsOr, List[String]]
  // pure, raiseError, handleError and handleErrorWith

  // Extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError, handleErrorWith
  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr]
  // requires the implicit ApplicativeError[ErrorsOr, List[String]]

  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover { case _ =>
    43
  }

  import cats.syntax.monadError._ // ensure method
  val testedSuccess: ErrorOr[Int] = success.ensure("Something bad")(_ > 10)

  def main(args: Array[String]): Unit = {}
}
