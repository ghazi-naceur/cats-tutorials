package gn.cats.tutorials.ch2.abstractmath

import scala.util.Try

// 3
object Functors {

  // Functor is a Cats Type Class that provides a 'map' method

  val aModifiedList: List[Int] = List(1, 2, 3).map(_ + 1) // List(2,3,4)
  val aModifiedOption: Option[Int] = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry: Try[Int] = Try(42).map(_ + 1) // Success(43)

  // Functor will generalize the idea of 'map' function

  // Simplified definition of a Functor
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]
  val listFunctor: Functor[List] = Functor[List]
  // Provide only the Higher Kinded Type F[_], so 'Functor[List]', not Functor[List[Int/String..etc]]

  val incrementedNumbers: List[Int] = listFunctor.map(List(1, 2, 3))(_ + 1) // List(2,3,4)

  import cats.instances.option._ // includes Functor[Option]
  val optionFunctor: Functor[Option] = Functor[Option]
  val incrementedOption: Option[Int] = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_._ // It's 'try_', not 'try', because that helps to not collide with the keyword 'try' in scala
  val tryFunctor: Functor[Try] = Functor[Try]
  val anIncrementedTry: Try[Int] = tryFunctor.map(Try(42))(_ + 1) // Success(43)

  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(tryInt: Try[Int]): Try[Int] = tryInt.map(_ * 10)

  // Generalize the API
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)
  // '(implicit functor: Functor[F])', adds the 'map' method

  // Custom Functor for a binary tree
  trait Tree[+T]
  object Tree {
    // defining smart constructors
    // Smart constructors return the General class (Tree[T]) and their implementation contains the subclasses (Leaf and Branch)
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value)                => Leaf(f(value))
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
    }
  }

  // Extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree: Tree[Int] = tree.map(_ + 1)
  // access to 'map' method, because of:
  //    - import cats.syntax.functor._
  //    - defining an implicit functor for Tree in this scope

  // A shorter version of 'do10x' method using the extension method 'map':
  def do10xSorter[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)
  // 'container.map(_ * 10)', because in the scope of this function, the compiler has access to an implicit functor of F,
  // and with the import cats.syntax.functor._, we have access to the extension method
  // [F[_]: Functor], meaning that there is an implicit functor of F in the scope of this method

  def main(args: Array[String]): Unit = {
    println(do10xList(List(1)))
    println(do10xOption(Option(1)))
    println(do10xTry(Try(1)))

    // Generalizing
    println(do10x(List(1)))
    println(do10x(Option(1)))
    println(do10x(Try(1)))

    println(do10x[Tree](Branch(30, Leaf(10), Leaf(20))))
    // We add the type [Tree] explicitly, because the parameter contains a Branch[Int], not a Tree[Int], and because our
    // TCs are invariant (Cats), the compiler can't find an implicit Functor[Branch], that's why we did specified do10x[Tree]
    // to help the compiler to find out that the expression is of type Tree[Int], not Branch[Int]. To adjust this issue,
    // we can the smart constructors defined in the companion object, and then we rewrite the previous instruction as follows:
    println(do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))

    // Generalizing and shorter
    println(do10xSorter(List(1)))
    println(do10xSorter(Option(1)))
    println(do10xSorter(Try(1)))
    println(do10xSorter(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))

    /*
    Use cases: Data structures meant to be transformed in sequence
      - specialized data structures for high-performance algorithms
      - any "mappable" structures under the same high-level API
     */
  }
}
