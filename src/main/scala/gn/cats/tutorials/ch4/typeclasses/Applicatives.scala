package gn.cats.tutorials.ch4.typeclasses

// 2
object Applicatives {

  // Applicative is an extension of Functor, that will introduce the 'pure' method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative: Applicative[List] = Applicative[List] // can be used to create a wrapped value from a normal value
  val aList: List[Int] = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // implicit Applicative[Option]
  val optionApplicative: Applicative[Option] = Applicative[Option]
  val anOption: Option[Int] = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._
  val anotherList: List[Int] = 2.pure[List] // List(2)
  val anotherOption: Option[Int] = 2.pure[Option] // Some(2)

  // Monad extends Applicative
  // Applicative extends Functor

  // Applicatives are rarely used by themselves, because most Applicatives are actually a stronger type (Monads type)
  // Most suitable example for Applicatives is Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(12) // "pure" method
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // "map" method (from Functor)
  val validatedApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr]

  // Thought experiment
  // W: Wrapper type
  // B: our input
  // T: Tuple
  def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T] = ???
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    ap(functionWrapper)(wb)
    // In the presence of an Applicative (implicit), you can create the 'product' fundamental method of a Semigroupal
    // only if you have the 'ap' method. The Applicative type has already the 'ap' method (so technically, we don't
    // need the 'ap' helper function, we can use the 'ap' defined inside Applicative):
    applicative.ap(functionWrapper)(wb)
    // Applicative can implement 'product' from Semigroupal
    // => Applicative extends Semigroupal
  }

  def main(args: Array[String]): Unit = {}
}
