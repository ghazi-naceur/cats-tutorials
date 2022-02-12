package gn.cats.tutorials.ch4.typeclasses

import cats.{Applicative, Apply}

// 4
object WeakerMonads {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(wf)(f => f(a)))
    // (wa) = M[A]
    // a = A
    // map(wf) = M[A => B]
    // f = A => B
    // f(a) = M[B]
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ // flatMap extension method
  import cats.syntax.functor._ // map extension method

  def getPairs[M[_]: FlatMap](numbers: M[Int], chars: M[Char]): M[(Int, Char)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  def getPairsGeneral[M[_]: FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  def main(args: Array[String]): Unit = {}
}
