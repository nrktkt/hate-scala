package hate

trait HalEmbeddable[T] {
  def toHalResource(obj: T): HalResource[T]
}
