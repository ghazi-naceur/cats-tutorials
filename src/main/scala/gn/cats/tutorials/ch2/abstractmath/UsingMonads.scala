package gn.cats.tutorials.ch2.abstractmath

import cats.Semigroup

import scala.util.{Failure, Success, Try}

// 5
object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  val monadList: Monad[List] = Monad[List] // fetches the implicit Monad[List]
  val aSimpleList: List[Int] = monadList.pure(2) // List(2)
  val anExtendedList: List[Int] = monadList.flatMap(aSimpleList)(x => List(x, x + 1)) // List(2,3)
  // applicable as well on Option, Try, Future

  // Either is also a Monad
  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]
  val anEither: LoadingOr[Int] = loadingMonad.pure(5) // Right(5)
  val aTransformedEither: LoadingOr[Int] =
    loadingMonad.flatMap(anEither)(x => if (x % 2 == 0) Right(x + 1) else Left("Loading in progress"))

  // Example: Online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data") else Right("Amsterdam, Netherlands")

  val orderId = 452L
  val orderLocation1: LoadingOr[String] =
    loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))

  // You can use extension methods
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  val orderLocation2: LoadingOr[String] = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))

  val orderLocation3: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: The service layer API for a web app
  case class Connection(host: String, port: String)
  val config = Map("host" -> "localhost", "port" -> "4040")

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }
  /*
  Requirements:
  - If the host and port are found in the configuration map, then we'll return a M containing a connection with those
     values otherwise the method will fail, according to the logic of the type M:
   * for Try it will return a Failure
   * for Option it will return a None
   * for Future it will be a failed future
   * for Either it will be Left
  - The issue request method returns a M containing the string: "request (payload) has been accepted", if the payload
     is less than 20 characters, otherwise the method will fail, according to the logic of type M

    TODO: provide a real implementation of HttpService using Try, Option, Future, Either
   */

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None else Some(s"request ($payload) has been accepted")
  }

  val responseOption: Option[String] =
    OptionHttpService.getConnection(config).flatMap { conn =>
      OptionHttpService.issueRequest(conn, "This is a payload")
    }

  val responseOptionFor: Option[String] = for {
    conn <- OptionHttpService.getConnection(config)
    result <- OptionHttpService.issueRequest(conn, "This is a payload")
  } yield result

  // TODO: implement another HttpService with LoadingOr or ErrorOr
  object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = {
      if (!cfg.contains("host") || !cfg.contains("port")) Left(new RuntimeException("Connection could not be establish"))
      else Right(Connection(cfg("host"), cfg("port")))
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("Too much payload"))
      else Right(s"request ($payload) has been accepted")
  }

  val responseErrorOr: ErrorOr[String] = AggressiveHttpService
    .getConnection(config)
    .flatMap(conn => AggressiveHttpService.issueRequest(conn, "This is a payload"))

  val responseErrorOrFor: ErrorOr[String] = for {
    conn <- AggressiveHttpService.getConnection(config)
    response <- AggressiveHttpService.issueRequest(conn, "This is a payload")
  } yield response

  // Generalize the API
//  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
  def getResponse[M[_]: Monad](service: HttpService[M], payload: String): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response

  def main(args: Array[String]): Unit = {

    println(responseOption)
    println(responseOptionFor)
    println(responseErrorOr)
    println(responseErrorOrFor)
    println(getResponse(OptionHttpService, "This is a payload"))
    println(getResponse(AggressiveHttpService, "This is a payload"))
  }
}
