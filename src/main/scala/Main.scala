import akka.http.scaladsl.model.Uri
import hate.HalExtractable.ContextualReader
import hate.HalLink._
import hate.HalResource._
import hate.{HalEmbeddable, HalExtractable, HalResource, UriString}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

object M extends App{

  case class PaymentAccount(`type`: String, info: String)
  case class Order(id: Int, total: Double, currency: String, status: String, paymentAccount: PaymentAccount, basketId: Int, customerId: Int)

  implicit val orderWriter: Writer[Order] = (obj: Order) =>
      (("id" -> obj.id) ~
        ("total" -> obj.total)) (org.json4s.JsonDSL.double2jvalue) ~
        ("currency" -> obj.currency) ~
        ("status" -> obj.status)


  implicit val accountFormat = new JsonFormat[PaymentAccount] {
    def read(value: JValue) = {
      for {
        JObject(obj) <- value
        JField("type", JString(typ)) <- obj
        JField("info", JString(info)) <- obj
      } yield PaymentAccount(typ, info)
    }.head

    def write(obj: PaymentAccount) =
      ("type", obj.`type`) ~ ("info", obj.info)
  }

  val orderReader: ContextualReader[Order] = (links, embeds) => { (json: JValue) =>
      val basketLink = """\/baskets\/(\d+)""".r
      val customerLink = """\/customers\/(\d+)""".r
      val f = for {
        JObject(order) <- json
        JField("id", JInt(id)) <- order
        JField("total", JDouble(total)) <- order
        JField("currency", JString(currency)) <- order
        JField("status", JString(status)) <- order
        ("basket", Right(HalHref(Right(UriString(basketLink(basketId)))))) <- links
        ("customer", Right(HalHref(Right(UriString(customerLink(customerId)))))) <- links
        ("account", Right(HalResource(account: PaymentAccount, _, _))) <- embeds
      }
        yield Order(id.toInt, total, currency, status, account, basketId.toInt, customerId.toInt)
      f.head
  }

  implicit val accountEmbeddable = HalEmbeddable[PaymentAccount]

  implicit def orderEmbeddable = HalEmbeddable( (obj: Order) =>
    HalResource(
      obj,
      embedded = Map("account" -> obj.paymentAccount),
      links = Map(
        "self" -> Uri(s"/orders/${obj.id}"),
        "basket" -> Uri(s"/baskets/${obj.basketId}"),
        "customer" -> Uri(s"/customers/${obj.customerId}")
      )
    )
  )

  val order = Order(123, 30, "USD", "shipped", PaymentAccount("PayPal", "jim@gmail.com"), 97212, 7809)
  val orderHal: HalResource[Order] = orderEmbeddable.toHalResource(order)
  val json: JValue = asJValue(orderHal)
  println(pretty(render(json)))

  implicit val accountExtractable = new HalExtractable[PaymentAccount] {
    def writer = accountFormat
    def reader = (_, _) => accountFormat
    def extractors = Map.empty
  }

  implicit val orderExtractable = new HalExtractable[Order] {
    val writer = orderWriter
    val reader = orderReader
    def extractors: Map[String, HalExtractable[_]] = Map("account" -> accountExtractable)
  }

  val parsed: HalResource[Order] = HalResource.fromJson[Order](json)

  println(parsed)


  val orders: Seq[Order] = Seq(order, Order(456, 3.50, "USD", "processing", PaymentAccount("Bank", "01234567890123"), 97213, 12369))
  val hal = HalResource(
    links = Map(
      "self" -> Uri("/orders"),
      "next" -> Uri("/orders?page=2")
    ),
    embedded = Map("orders" -> orders)
  )

  println(pretty(render(asJValue(hal))))
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