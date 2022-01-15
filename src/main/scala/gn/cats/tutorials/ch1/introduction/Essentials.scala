package gn.cats.tutorials.ch1.introduction

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object Essentials extends App {

  // Higher order functions can receive functions as arguments or return other functions as results, like: map, flatMap,
  // filter ... etc

  // scala 2.12:
//  import scala.concurrent.ExecutionContext.Implicits.global
  // Starting from scala 2.13:
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))

  val combiner1: List[(Int, Char)] = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c)))
  val combiner2: List[(Int, Char)] = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c)

  println(combiner1)
  println(combiner2)

  // Partial function
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 11
    case 2 => 22
    case 3 => 33
  }

  // Higher kinded types
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }
}
