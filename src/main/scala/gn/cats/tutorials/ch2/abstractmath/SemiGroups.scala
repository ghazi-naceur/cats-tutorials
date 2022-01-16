package gn.cats.tutorials.ch2.abstractmath

// 1
object SemiGroups {

  // Semigroups combine elements of the same type
  import cats.Semigroup

  import cats.instances.int._
  val naturalIntSemiGroup: Semigroup[Int] = Semigroup[Int]
  val intCombination: Int = naturalIntSemiGroup.combine(4, 5) // addition

  import cats.instances.string._
  val naturalStringSemiGroup: Semigroup[String] = Semigroup[String]
  val stringCombination: String = naturalStringSemiGroup.combine("isaac", "netero") // concatenation

  // Specific APIs
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemiGroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemiGroup.combine)

  // General API
  // The compiler will look for the implicit semigroup to use in the scope
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  case class Expense(id: Long, amount: Double)
  implicit val expenseSemiGroup: Semigroup[Expense] = Semigroup.instance[Expense] { (x, y) =>
    Expense(Math.max(x.id, y.id), x.amount + y.amount)
  }

  // Extension method from Semigroup: |+|
  import cats.syntax.semigroup._
  val anIntSum: Int = 2 |+| 3
//  val invalidIntSum: Int = 2 |+| "a string" // won't compile because can't combine 2 elements with different types
  // requires the presence of an implicit Semigroup[Int]

  val aStringConcat: String = "Isaac " |+| "Netero"
  // requires the presence of an implicit Semigroup[String]

  val anExpenseCombine: Expense = Expense(1, 10.10) |+| Expense(2, 20.20)
  // requires the presence of an implicit Semigroup[Expense]

//  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)
  // shorter version:
  // [T : Semigroup] : meaning that the compiler needs to have an implicit Semigroup of type T
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // Specific APIs
    val ints = (1 to 10).toList
    println(reduceInts(ints))
    val strings = List("Isaac ", "Netero ", "is ", "an ", "old ", "man")
    println(reduceStrings(strings))

    // General API
    println(reduceThings(ints)) // compiler injects the implicit SemiGroup[Int]
    println(reduceThings(strings)) // compiler injects the implicit SemiGroup[String]

    import cats.instances.option._
    // compiler will produce an implicit SemiGroup[Option[Int]]
    // compiler will produce an implicit SemiGroup[Option[String]]

    val numberOptions: List[Option[Int]] = ints.map(n => Option(n))
    println(reduceThings(numberOptions)) // an Option[Int] containing the sum of all numbers
    // Implicit TC instances for Int and Option are already in the scope

    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
    println(reduceThings(stringOptions))

    val expense1 = Expense(1, 10.20)
    val expense2 = Expense(2, 20.20)
    val expense3 = Expense(3, 30.20)
    val expenses = List(expense1, expense2, expense3)

    println(reduceThings(expenses))

    println(reduceThings2(ints))
    println(reduceThings2(strings))
    println(reduceThings2(numberOptions))
    println(reduceThings2(stringOptions))
    println(reduceThings2(expenses))

    /*
    Use cases for Semigroup:
      - data integration & big data processing
      - eventual consistency & distributed computing
     */
  }
}
