package gn.cats.tutorials.ch3.datamanipulation

import cats.Id
import cats.data.Kleisli

// 1
object Readers {

  /** Major type classes:
    *  1- Semigroup: |+|
    *  2- Monoid extends Semigroup: empty, |+|
    *  3- Functor: map
    *  4- Monad extends Functor: pure, flatMap
    */

  /*
      - configuration file => initial data structure
      - a DB layer
      - an HTTP layer
      - a business logic layer
   */

  case class Configuration(
    dbUsername: String,
    dbPassword: String,
    host: String,
    port: Int,
    nbThreads: Int,
    emailReplyTo: String
  )
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched"
    // select * from the db table and return the status of the orderId

    def getLastOrderId(username: String): Long = 6543
    // select max(orderId) from table where username = username
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server
  }

  // bootstrap
  val config: Configuration = Configuration("isaac", "netero", "localhost", 1234, 6, "isaac.netero@hxh.com")
  // Cats Reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  /*
    Reader[Configuration, DbConnection]
    Reader[I, O]
    with:
      Configuration as input
      DbConnection as output
   */
  val dbConnection: Id[DbConnection] = dbReader.run(config)

  val isaacOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(5))
  val issacOrderStatus: String = isaacOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
//    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
//      .map(_.getLastOrderId(username))
//      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))
    // The same as:
    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }

  /*
    Pattern:
      1- You create the initial data structure
      2- You create a reader which specifies how that data structure will be manipulated later
      3- You can then map & flatMap the reader to produce derived information
      4- When you need the final piece of information, you call `run` on the reader with the initial data structure
   */

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String = s"From '$emailReplyTo'; To '$address' >>> '$contents'"
  }

  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of their last order
    // email them with the Email Service: "Your last order has the status: 'status'"

    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))
    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your last order has the status: '$orderStatus'")

    emailReader.run(config)
  }

  // Dependency injection in purely functional way using Cats

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("isaac"))
    println(emailUser("isaac", "beyond.netero@hxh.com"))
  }
}
