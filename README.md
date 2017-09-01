```scala
case class Order(id: Int, total: Double, currency: String, status: String, basket: Basket, customer: Customer)

implicit val orderWriter: Writer[Order] = ???

implicit val basketLinkable: HalLinkable[Basket] = ???
implicit val customerLinkable: HalLinkable[Customer] = ???

implicit val orderEmbeddable = new HalEmbeddable[Order] {
  def toHalResource(obj: Order) = HalResource(
    obj,
    links = Map(
      "self" -> HalLink(Uri(s"/orders/${obj.id}")),
      "basket" -> obj.basket,
      "customer" -> obj.customer
    )
  )
}

val order = Order(123, 30, "USD", "shipped", 97212, 7809)
val orderHal = orderEmbeddable.toHalResource(order)
println(pretty(asJValue(orderHal)))
```
```json
{
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
  }
}
```  