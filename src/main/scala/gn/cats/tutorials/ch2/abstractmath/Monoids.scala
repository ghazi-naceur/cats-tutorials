package gn.cats.tutorials.ch2.abstractmath

// 2
object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // importing the |+| extension method

  val numbers: List[Int] = (1 to 1000).toList
  // |+| is always associative
  val sumLeft: Int = numbers.foldLeft(0)(_ |+| _)
  val sumRight: Int = numbers.foldRight(0)(_ |+| _)

  // Define a general API
//  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.foldLeft()(_|+|_)
  // => Semigroup is not enough to define a general API in this case. We're missing the initial value as a seed for the
  // fold method. What's the initial value of type T? that's the question! Semigroup is not enough, because in a generic
  // manner, Semigroup doesn't specify an empty value (initial value).
  // To fix this issue, we need to use "Monoid".

  // Monoid
  import cats.Monoid
  // It has the same capabilities as a Semigroup + an empty value
  val intMonoid: Monoid[Int] = Monoid[Int]
  val combineInt: Int = intMonoid.combine(12, 24) // Same impl as a Semigroup[String]
  val zero: Int = intMonoid.empty // 0 value for Int

  import cats.instances.string._ // bring the implicit Monoid[String] in scope
  val stringMonoid: Monoid[String] = Monoid[String]
//  val emptyString: String = stringMonoid.empty // ""
  val emptyString: String = Monoid[String].empty // ""
  val combineString: String = Monoid[String].combine("Isaac ", "Netero") // Same impl as a Semigroup[String]
  // Monoid trait is extending the Semigroup trait

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]] and Monoid[Option[String]]
  val emptyOption: Option[Int] = Monoid[Option[Int]].empty // None
  val combineOption: Option[Int] = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Option(2)

  // Extension methods for Monoid - |+| (from import cats.syntax.semigroup._)
  val combineOptionFancy: Option[Int] = Option(3) |+| Option(2)
  // You can import cats.syntax.monoid._, which will provide you with the same extension method |+| from semigroup
//  import cats.syntax.monoid._
  val combineOptionFancy2: Option[Int] = Option(3) |+| Option(2)

  // Generic
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldLeft(monoid.empty)(_ |+| _)

  // Case class Monoid
  case class ShoppingCart(items: List[String], total: Double)
  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
    ShoppingCart(List(), 0.0),
    (cartA, cartB) => ShoppingCart(cartA.items ++ cartB.items, cartA.total + cartB.total)
  )
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)

    val ints = (1 to 1000).toList
    println(combineFold(ints))

    val strings = List("Isaac ", "Netero ", "is ", "an ", "old ", "man")
    println(combineFold(strings))

    // Combine a list of phone books as Maps[String, Int] to a List of a single Map[String, Int]
    val phoneBooks = List(
      Map("Isaac" -> 123, "Shisui" -> 234),
      Map("Itachi" -> 345, "Takamora" -> 456),
      Map("Ging" -> 567, "Silver" -> 678)
    )
    import cats.instances.map._ // Monoid[Map] with an empty value: Map()
    val phonebooksCombined = phoneBooks.foldLeft(Monoid[Map[String, Int]].empty)(_ ++ _)
    // or using |+|:
    val phonebooksCombined2 = combineFold(phoneBooks)

    println(phonebooksCombined)
    println(phonebooksCombined2)

    val shoppingCarts = List(
      ShoppingCart(List("chair", "table", "tv"), 1500.0),
      ShoppingCart(List("sofa", "desk"), 1000.0),
      ShoppingCart(List("dishes"), 100.0)
    )
    println(checkout(shoppingCarts))
  }
}
