package gn.cats.tutorials.ch4.typeclasses

import cats.{Eval, Monoid}

// 6
object Folding {

  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((a, currentList) => f(a) :: currentList)
    // foldRight: from last element + prepending using '::' to obtain the initial order

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
//      list.foldLeft(List.empty[B])((currentList, a) => currentList ++ f(a)) // or :
      list.foldLeft(List.empty[B])((currentList, a) => currentList.foldRight(f(a))(_ :: _))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((a, currentList) => if (predicate(a)) a :: currentList else currentList)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldLeft(Monoid.empty)(monoid.combine)
  }

  import cats.Foldable
  import cats.instances.list._ // import Foldable[Int]
  val sum: Int = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option._ // import Foldable[Option]
  val sumOption: Int = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }
  // Using Eval makes the API stack-safe == foldRight is stack-safe (recursion), regardless of the impl of your container

  import cats.instances.int._
  val anotherSum: Int = Foldable[List].combineAll(List(1, 2, 3)) // implicit Monoid[Int]

  import cats.instances.string._
  val mappedConcat: String = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // implicit Monoid[String]

  // Nesting:
  import cats.instances.vector._
  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // Extension methods:
  import cats.syntax.foldable._
  val sum3: Int = List(1, 2, 3).combineAll // requires implicits: Foldable[List] and Monoid[Int]
  val mappedConcat2: String = List(1, 2, 3).foldMap(_.toString) // requires implicits: Foldable[List] and Monoid[String]

  def main(args: Array[String]): Unit = {

    import ListExercises._
    val numbers = (1 to 10).toList
    println(map(numbers)(_ + 1))
    println(flatMap(numbers)(x => (1 to x).toList))
    println(filter(numbers)(_ % 2 == 0))

    import cats.instances.int._ // Monoid[Int]
    println(combineAll(numbers))
  }
}
