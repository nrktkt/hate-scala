## Server Usage (hypermedia from models)

```scala
case class PaymentAccount(`type`: String, info: String)
case class Order(id: Int, total: Double, currency: String, status: String, paymentAccount: PaymentAccount, basketId: Int, customerId: Int)

// json4s typeclasses for converting your models to json
implicit val orderWriter: Writer[Order] = ??? // a json4s Writer
implicit val accountFormat: JsonFormat[PaymentAccount] = ??? // a json4s JsonFormat

// hate readers provide links and embeds when converting from json, in case information from them is needed in your models 
val orderReader: ContextualReader[Order] = (links, embeds) => new Reader[Order] {
  def read(json: JValue) = {
    val basketLink = """\/baskets\/(\d+)""".r
    val customerLink = """\/customers\/(\d+)""".r
    for {
      JObject(order) <- json
      JField("id", JInt(id)) <- order
      JField("total", JDouble(total)) <- order
      JField("currency", JString(currency)) <- order
      JField("status", JString(status)) <- order
      ("basket", Right(HalHref(Right(UriString(basketLink(basketId)))))) <- links
      ("customer", Right(HalHref(Right(UriString(customerLink(customerId)))))) <- links
      ("account", Right(HalResource(account: PaymentAccount, _, _))) <- embeds
    } yield Order(id.toInt, total, currency, status, account, basketId.toInt, customerId.toInt)
  }
}
  
// resources with no links or embedded resources can be created easily as below
implicit val accountEmbeddable = HalEmbeddable[PaymentAccount]
  
implicit def orderEmbeddable = HalEmbeddable( (obj: Order) =>
  HalResource(
    obj,
    embedded = Map("account" -> obj.paymentAccount),
    links = Map(
      "self" -> Uri(s"/orders/${obj.id}"),
      "basket" -> Uri(s"/baskets/${obj.basketId}"),
      "customer" -> Uri(s"/customers/${obj.customerId}")
    )))

val order = Order(123, 30, "USD", "shipped", PaymentAccount("PayPal", "jim@gmail.com"), 97212, 7809)
val orderHal: HalResource[Order] = orderEmbeddable.toHalResource(order)
val json: JValue = asJValue(orderHal)
println(pretty(render(json)))
```
```json
{
  "id" : 123,
  "total" : 30.0,
  "currency" : "USD",
  "status" : "shipped",
  "_links" : {
    "self" : {
      "href" : "/orders/123"
    },
    "basket" : {
      "href" : "/baskets/97212"
    },
    "customer" : {
      "href" : "/customers/7809"
    }
  },
  "_embedded" : {
    "account" : {
      "type" : "PayPal",
      "info" : "jim@gmail.com"
    }
  }
}
```  

## Client Usage (models from hypermedia)

```scala
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
val order: Order = parsed.value
```

## Maybe make some paginated lists

```scala
val orders: Seq[Order] = Seq(order, Order(456, 3.50, "USD", "processing", PaymentAccount("Bank", "01234567890123"), 97213, 12369))
val hal = HalResource(
  links = Map(
    "self" -> Uri("/orders"),
    "next" -> Uri("/orders?page=2")
  ),
  embedded = Map("orders" -> orders)
)
```

```json
{
  "_links" : {
    "self" : {
      "href" : "/orders"
    },
    "next" : {
      "href" : "/orders?page=2"
    }
  },
  "_embedded" : {
    "orders" : [ {
      "id" : 123,
      "total" : 30.0,
      "currency" : "USD",
      "status" : "shipped",
      "_links" : {
        "self" : {
          "href" : "/orders/123"
        },
        "basket" : {
          "href" : "/baskets/97212"
        },
        "customer" : {
          "href" : "/customers/7809"
        }
      },
      "_embedded" : {
        "account" : {
          "type" : "PayPal",
          "info" : "jim@gmail.com"
        }
      }
    }, {
      "id" : 456,
      "total" : 3.5,
      "currency" : "USD",
      "status" : "processing",
      "_links" : {
        "self" : {
          "href" : "/orders/456"
        },
        "basket" : {
          "href" : "/baskets/97213"
        },
        "customer" : {
          "href" : "/customers/12369"
        }
      },
      "_embedded" : {
        "account" : {
          "type" : "Bank",
          "info" : "01234567890123"
        }
      }
    } ]
  }
}
```