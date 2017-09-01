package hate

trait HalLinkable[T] {
  def toHalLink(obj: T): HalLink
}
