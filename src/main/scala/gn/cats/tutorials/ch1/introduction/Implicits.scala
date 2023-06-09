package gn.cats.tutorials.ch1.introduction

// 1
object Implicits extends App {

  // 1- implicit classes: 1 argument wrappers over values
  case class Person(name: String) {
    def greet: String = s"Name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  // Explicit method to call the Person greet function
  val impersonableString = new ImpersonableString("Isaac")
  impersonableString.greet

  // Implicit call for the Person greet method
  val greeting: String = "Isaac".greet

  // 2- implicit arguments and values: proving the existence of a type that will be used by the compiler
  def increment(x: Int)(implicit amount: Int): Int = x + amount
  implicit val defaultAmount: Int = 10

  val incrementer: Int = increment(5) // implicit argument 10 is passed by the compiler
  println(incrementer)

  // Complex example:
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer: JsonSerializer[Person] = new JsonSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         | { "name": "${person.name}" }
         |""".stripMargin
  }
  val personJson = listToJson(List(Person("Isaac"), Person("Itachi"))) //(personSerializer)
  println(personJson)

  // 3- implicit methods: if the code is applicable to more than just a case class with a single argument, we use an
  // implicit method instead

  // Product: Type that all case classes extend
  // .productElementName: is the name of the case class
  // .productElement: is the value of the case class
  // This method is more general than JsonSerializer, for which we need a serializer for each case class
  implicit def oneArgCaseClassSerializer[T <: Product]: JsonSerializer[T] = new JsonSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{ "${value.productElementName(0)}": "${value.productElement(0)}" }
         |""".stripMargin.trim
  }
//
//  implicit val animalSerializer: JsonSerializer[Animal] = oneArgCaseClassSerializer[Animal]
  case class Animal(name: String)

//  println(oneArgCaseClassSerializer[Animal].toJson(Animal("Kyubi")))
//  println(oneArgCaseClassSerializer[Person].toJson(Person("Naruto")))
//  println(Animal("kyubi").toJson)
  println(listToJson(List(Animal("Ichibi"), Animal("Rokobi"), Animal("Kyubi"))))

  /*
  You must have exactly one implicit of that type in scope.
  The compiler searches for implicits in this order:
    1- the local scope = clearly defined implicit vals/defs/classes
    2- the imported scope
    3- the companion objects of the types involved in the method call
      eg: List(1,2,3).sorted
        The implicit method 'sorted' is imported from the companion object of Int. In fact, the compiler will try to search
        for 'sorted' in the local scope, then the imported scope, finally in the companion objects of List or Int.
   */
}
