package hate

import hate.HalExtractable.ContextualReader
import hate.HalResource.{HalEmbeds, HalLinks}
import org.json4s.{Reader, Writer}

trait HalEmbeddable[T] {
  def toHalResource(obj: T): HalResource[T]
}

object HalEmbeddable {
  def apply[T](f: T => HalResource[T]) = new HalEmbeddable[T] {
    def toHalResource(obj: T) = f(obj)
  }

  def apply[T](implicit writer: Writer[T]): HalEmbeddable[T] = HalEmbeddable((t: T) => HalResource(t)(writer))
}

object HalExtractable {
  type ContextualReader[T] = (HalLinks, HalEmbeds) => Reader[T]
}

trait HalExtractable[T] {
  def writer: Writer[T]
  def reader: ContextualReader[T]
  def extractors: Map[String, HalExtractable[_]]
}