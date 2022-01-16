package gn.cats.tutorials.ch1.introduction

// 4
object CatsEqIntro {

  // Eq: Cats Type Class to compare values at compile time and make the code not compile if the values that you're comparing
  // are of different types. It insures Type Safe Equality.
  val aComparing: Boolean = 2 == "a string" // compiler warning
  // code compiles and returns false, because the values from different types. It's better to make code not compile at all *
  // in this case: Use 'Eq' Type Class

  // Part 1 - Type Class import
  import cats.Eq

  // Part 2 - Import Type Class instances for the types you need
  import cats.instances.int._

  // Part 3 - Use Type Class API
  val intEquality: Eq[Int] = Eq[Int]
  val aTypeSafeComparison: Boolean = intEquality.eqv(4, 5) // returns false
//  val unSafeComparison: Boolean = intEquality.eqv(4, "a string") // doesn't compile

  // Part 4 - Use extension methods (if applicable)
  import cats.syntax.eq._
  // We imported the type class (part 1) and its instances (part 2), so we can use the extension methods (if already implemented)
  // '===' is an extension provided by cats
  val anotherTypeSafeComparison: Boolean = 2 === 3 // returns false
  val neqComparison: Boolean = 2 =!= 3 // returns true
//  val invalidComparison = 2 === "a string" // doesn't compile
  // Extension methods are only visible in the presence of the right Type Class instance ('cats.instances.int._' in our case)

  // Part 5 - Extending the Type Class operations to composite types, eg: lists
  import cats.instances.list._ // We bring 'Eq[List]' in scope, and we had already imported 'cats.instances.int._',
  // so the compiler has 'Eq[List[Int]]' in scope
  val aListComparison: Boolean = List(2) === List(3) // false

  import cats.instances.string._
  val aListStringComparison: Boolean = List("a string") === List("a second string")

  // Part 6 - Create a Type Class instance for a custom type
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars: Boolean = ToyCar("Mercedes", 20.10) === ToyCar("Audi", 19.20)

  /*
  import cats.YourTypeClass // use your type class
  import cats.instances.yourType._ // bring implicit TC instances for your supported type in scope
  import cats.syntax.yourTypeClass._ // use extension methods your TC supports

  Some imports are not self-evident, so you need to import all:
  import cats._
  import cats.implicits._

   */
}
