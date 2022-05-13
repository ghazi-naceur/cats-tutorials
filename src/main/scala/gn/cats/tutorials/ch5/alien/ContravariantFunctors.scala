package gn.cats.tutorials.ch5.alien

import cats.Monoid

// 2
object ContravariantFunctors {

  trait Format[T] { self =>
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[T](value: T)(implicit f: Format[T]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // Problem: Given Format[MyType], can we also have a Format[Option[MyType]] (or a Format[List[MyType]] ...etc) ?
  import cats.instances.option._ // to compile Option[Option[Int]]
  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))
  //    new Format[Option[T]] {
  //    override def format(value: Option[T]): String = f.format(value.get)
  //  }

  /*
    IntFormat
    fo1: Format[Option[Int]] = IntFormat.contramap[Option[Int]])(_.get) // first get
    fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]])(_.get) // second get

    fo2 = IntFormat
            .contramap[Option[Int]])(_.get) // first get
            .contramap[Option[Option[Int]]])(_.get) // second get

     fo2.format(Option(Option(12))) =
          fo1.format(secondGet(Option(Option(12))) =
          IntFormat.format(firstGet(secondGet(Option(Option(12))))

     Order of operations:
        - second get
        - first get
        - format of Int
        => The execution order is in reverse from the written order

     Map applies transformations in sequence
     Contramap applies transformations in reverse sequence => Contravariant type class
   */

  import cats.Contravariant
  import cats.Show // .format
  import cats.instances.int._ // implicit Show[Int]
  val showInts: Show[Int] = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  // extension methods
  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("This is an announcement"))
    println(format(12))
    println(format(true))
    println(format(Option(12)))
    println(format(Option(Option(12))))
  }
}
