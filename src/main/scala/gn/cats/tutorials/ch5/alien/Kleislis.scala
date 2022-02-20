package gn.cats.tutorials.ch5.alien

object Kleislis {

  val plainFunc1: Int => String = x => if (x % 2 == 0) s"$x is even" else "fail"
  val plainFunc2: Int => Int = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  /*
    A Kliesli is a generic data structure that will help when composing functions returning wrapper instances, or Higher
    Kinded instances
   */
  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

//  val func3 = func2 andThen func1 // ==> We need a Kliesli

  import cats.data.Kleisli
  import cats.instances.option._ // FlatMap[Option]

  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience
  val multiply: Kleisli[Option, Int, Int] = func2K.map(_ * 2) // x => Option(...).map(_ * 2)
  val chain: Kleisli[Option, Int, String] = func2K.flatMap(x => func1K)

  // Exercise
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  val time2: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](x => x * 2)
  val plus4: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](x => x + 4)
  val composed: Kleisli[Id, Int, Int] = time2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor: Kleisli[Id, Int, Int] = for {
    t2 <- time2
    p4 <- plus4
  } yield t2 + p4

  import cats.data.Reader
  val time2R: Reader[Int, Int] = Reader[Int, Int](x => x * 2)
  val plus4R: Reader[Int, Int] = Reader[Int, Int](x => x + 4)
  val composedForR: Reader[Int, Int] = for {
    t2 <- time2R
    p4 <- plus4R
  } yield t2 + p4

  // 'InterestingKleisli' is actually a Reader. Reader is based on ReaderT, which is a Kleisli

  def main(args: Array[String]): Unit = {
    println(composedFor(3))
    println(composedForR(3))
  }
}
