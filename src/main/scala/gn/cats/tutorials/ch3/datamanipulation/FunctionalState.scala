package gn.cats.tutorials.ch3.datamanipulation

import cats.Eval
import cats.data.IndexedStateT

// 4
object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  // Basically a State is a wrapper on a single function
  val (eleven, counted10) = countAndSay.run(10).value
  // state = "iterative" computations

  // iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied by 5, obtained $a"

  // pure FP with states
  val firstTransformation: State[Int, String] = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation: State[Int, String] = State((s: Int) => (s * 5, s"Multiplied by 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] =
    firstTransformation.flatMap(firstResult => secondTransformation.map(secondResult => (firstResult, secondResult)))

  val compositeTransformation2: State[Int, (String, String)] = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  val func1: Int => (Int, String) = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2: Int => (Int, String) = (s: Int) => (s * 5, s"Multiplied by 5, obtained ${s * 5}")
  val compositeFunc: Int => (String, (Int, String)) = func1.andThen { case (newState, firstResult) =>
    (firstResult, func2(newState))
  }

  // An online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { cart =>
    (ShoppingCart(item :: cart.items, cart.total + price), price + cart.total)
  }

  val isaacCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("Fender guitar", 500)
    _ <- addToCart("Elixir strings", 19)
    total <- addToCart("Electric table", 7)
  } yield total

  // Pure mental gymnastics:
  // returns a State data structure that, when run, will not change the state but will issue the vale f(a)
  def inspect[A, B](f: A => B): State[A, B] = State((a: A) => (a, f(a)))

  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State((a: A) => (a, a))

  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State((_: A) => (value, ()))

  // returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State((a: A) => (f(a), ()))

  // => 'inspect', 'get', 'set' and 'modify' are already implemented in the companion object of State:
  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)
  // sequential computation in FP

  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(compositeTransformation2.run(10).value)
    println(compositeFunc(10))
    println(isaacCart.run(ShoppingCart(List(), 0)).value)
    println(program.run(5).value)
  }
}
