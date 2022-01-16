package gn.cats.tutorials.ch1.introduction

// 5
object TCVariance {

  import cats.Eq
  import cats.instances.int._ // Eq[Int] TC instance
  import cats.instances.option._ // constructs Eq[Option[Int]] TC instance
  import cats.syntax.eq._ // able to use '==='

  val aComparison: Boolean = Option(2) === Option(3)
//  val invalidComparison = Some(2) === None //  Eq[Some[Int]] is not found, even though Some is a subtype of Option.
  // This is related to variance

  // Variance
  class Animal
  class Cat extends Animal

  // Covariance type: subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // 'Cat <: Animal', so 'Cage[Cat] <: Cage[Animal]'

  // Contravariant type: subtyping is propagated backwards to the generic type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // 'Cat <: Animal', so 'Vet[Animal] <: Vet[Cat]'

  // Rule of thumb:
  //   - "Has a T" = covariant
  //   - "Acts on T" = contravariant

  // Variance affects how TC instances are being fetched

  // Contravariant TC:
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("something")
  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat] // ok - TC instance for Animal is also applicable to Cat
  // Rule 1: Contravariant TCs can use the superclass instances if nothing is available strictly for that type

  // This has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]] // ok
  makeSound[Some[Int]] // ok - With Covariance we fix the issue at the start of this tutorial for val 'invalidComparison'
  // Eq isn't Contravariant. It's Invariant.

  // Covariant TC:
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }
  implicit object CatShow extends AnimalShow[Cat] {
    override def show: String = "it's a cat"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show
  // Rule 2: Covariant TCs will always use the more specific TC instance for that type, but may confuse the compiler if
  // the general TC is also present

  // Rule 3: You can't have Rule 1 and Rule 2 in the same code
  // Cats uses Invariant TCs (like in Eq)
  Option(2) === Option.empty[Int] // ok - Using Smart constructors 'Option.empty[Int]', instead of using 'None'

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // ok - compiler will use the 'CatShow' implicit object
//    println(organizeShow[Animal]) // won't compile, because there is ambiguous implicits: In fact, the compiler sees
//    2 potential instances for AnimalShow[Animal] (1 for Animal, and 1 for Cat)
  }
}
