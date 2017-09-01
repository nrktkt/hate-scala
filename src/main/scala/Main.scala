import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.{Authority, Path}
import akka.http.scaladsl.model.{HttpRequest, Uri}
import hate.{HalEmbeddable, HalLink, HalResource}
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import HalResource._
import HalLink._

object M extends App{
  case class Sample(str: String, i: Int)

  case class Order(id: Int, total: Double, currency: String, status: String, basketId: Int, customerId: Int)

  implicit val orderWriter = new Writer[Order] {
    def write(obj: Order) =
      ("id" -> obj.id) ~
        ("total" -> obj.total) ~
        ("currency" -> obj.currency) ~
        ("status" -> obj.status)
  }

  implicit val orderEmbeddable = new HalEmbeddable[Order] {
    def toHalResource(obj: Order) = HalResource(
      obj,
      links = Map(
        "self" -> HalLink(Uri(s"/orders/${obj.id}")),
        "basket" -> HalLink(Uri(s"/baskets/${obj.basketId}")),
        "customer" -> HalLink(Uri(s"/customers/${obj.customerId}"))
      ))
  }

  val order = Order(123, 30, "USD", "shipped", 97212, 7809)
  val orderHal = orderEmbeddable.toHalResource(order)
  println(pretty(render(asJValue(orderHal))))

  implicit val ser = new Writer[Sample] {
    def write(obj: Sample): JValue = obj.str -> obj.i
  }
  val hal = HalResource(Sample("hi", 5)).copy(embedded = Map("other" -> Right(HalResource(Sample("lo", 4)))))
  println(pretty(render(hal.toJson)))
}

/*
case class `?exp`(vars: String*)

/**
  * Created by nfischer on 4/5/2017.
  */
object Main extends akka.http.scaladsl.client.RequestBuilding with Directives {
  def main(args: Array[String]): Unit = {

    val nextRef = "/orders?offset=100{&filled,limit}"
    val nextTemplated = UriTemplate(nextRef)
    // .render returns Uri
    val request = Get(nextTemplated("limit" -> 150).render.withAuthority(Authority.parse("somesite.com")))
    Http().singleRequest(request)


    val offset = 100

    // template building from string interpolation
    val nextTemplate = uriT"/orders{?offset,filled,limit}"
    // template building from dsl
    val nextTemplate = Path / "orders" `?exp`("offset", "filled", "limit")

    val returnToCaller = nextTemplate("offset" -> offset)


    // reverse routing from directives
    get {
      path("orders") {
        parameters('offset.as[Int], 'limit.as[Int], 'filled.as[Boolean]) { (offset, limit, filled) =>
          templated { template => // template is equivalent to uriT"/orders{?offset,limit,filled}"
            complete(s"self ref is: ${template("offset" -> offset, "limit" -> limit, "filled" -> filled)}!")
          }
        }
      }
    }
  }
}

trait UriTemplate {
  def render: Uri
  // expand any number of expressions, turning them into literals with the given values
  def apply(expansions: (String, String)*): UriTemplate
}
object UriTemplate {
  def parse(template: String): UriTemplate = ???
  def apply(template: String) = parse(template)
}

sealed trait Syntax {
  def toStringBuilder
}

case class Literal(value: String) extends Syntax {
  HttpRequest
}

sealed trait Expression extends Syntax {
  val op: Char
  val variables: Seq[String]
}

case class Template(template: Seq[Syntax]){
  override lazy val toString = {
    val sb = new StringBuilder
    for(s <- template){
      sb ++= s.toString
    }
    sb.mkString
  }
}

abstract case class Thing(s: String){
  def f: Int
}

case class ConcreteThing(override val s: String, f: Int) extends Thing(s)

case class Thing2(override val s: String) extends Thing(s) {
  val f = 5
}

//val f = Thing("hi")
*/