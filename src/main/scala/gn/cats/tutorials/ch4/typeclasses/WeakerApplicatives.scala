package gn.cats.tutorials.ch4.typeclasses

import cats.{Functor, Semigroupal}

// 3
object WeakerApplicatives {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    // mapN implementation
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2) // W[(A, B)]
      map(tupleWrapper) { case (a, b) =>
        f(a, b)
      }
    }

    def ap[B, T](wf: W[B => T])(wa: W[B]): W[T] // fundamental method
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] // fundamental method
  }

  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]
  val applyOption: Apply[Option] = Apply[Option]
  val funcApp: Option[Int] = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._ // extension methods from Apply
  val tupleOfOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionOfTuple: Option[(Int, Int, Int)] = tupleOfOptions.tupled // Some((1,2,3))
  val sumOption: Option[Int] = tupleOfOptions.mapN(_ + _ + _) // Some(6)

  // Weaker Applicative = Functor + Semigroupal + ap method

  def main(args: Array[String]): Unit = {}
}
