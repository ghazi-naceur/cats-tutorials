package gn.cats.tutorials.ch5.alien

import cats.Monoid

// 3
object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  // How to support this logic for Int, Double, Option[String]... ?
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  // Support Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  // Generalization: If you have a Crypto[T] => Crypto[Option[T]] if you have a Monoid[T] in scope
  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  import cats.Invariant
  import cats.Show // prints things to console
  import cats.instances.string._
  val showString: Show[String] = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))
// Identical to:
  import cats.syntax.invariant._
  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse(""))

  // What's the relationship between: invariant, contravariant and functor ?
  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }
  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = contramap(wa)(back)
  }
  trait MyFunctor[W[_]] extends MyInvariant[W] {
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)
  }
  // MyInvariant is the super type of MyContravariant and MyFunctor
  // MyFunctor is known as well as "Covariant" functor

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("some token")
    val decrypted = decrypt[String](encrypted)
    // Providing type [String], so the compiler can fetch the implicit Crypto[String]

    println(encrypted)
    println(decrypted)
    println(encrypt(Math.PI))
    // Compiler can easily find  Crypto[Double], thanks to the defined 'implicit val doubleCrypto'

    println(decrypt[Double](encrypt(Math.PI)))

    println(encrypt(Option("Let's encrypt")))
    // using Some instead of Option is not going to work because there is no implicit
    println(decrypt[Option[String]](encrypted))

    import cats.instances.double._
    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Math.PI)))
  }
}
