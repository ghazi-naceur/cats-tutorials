package gn.cats.tutorials.ch1.introduction

// 3
object TypeClasses {

  case class Person(name: String, age: Int)

  // Part 1 - Type class definition (generic trait or abstract class | enhancing some type with new capabilities)
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  // Part 2 - Create implicit type class instances (concrete implementations)
  implicit object StringSerializer extends JsonSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }
  implicit object IntSerializer extends JsonSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }
  implicit object PersonSerializer extends JsonSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         | {"name": "${value.name}", "age": ${value.age}}
         |""".stripMargin.trim
  }

  // Part 3 - Offer some API (implicit user)
  def convertListToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  // Part 4 - Extending the existing types via extension methods
  object JsonSyntax {
    implicit class JsonSerializable[T](value: T)(implicit serializer: JsonSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    println(convertListToJson(List("Isaac", "Shisui", "Itachi")))
    println(convertListToJson(List(1, 2, 3, 4, 5)))
    println(convertListToJson(List(Person("Isaac", 125), Person("Shisui", 29), Person("Itachi", 27))))

    import JsonSyntax._
    println(Person("Isaac", 125).toJson)
  }
}
