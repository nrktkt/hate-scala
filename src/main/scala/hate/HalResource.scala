package hate

import hate.HalResource._
import org.json4s.JsonDSL._
import org.json4s._

case class HalResource[T](
                           value: T,
                           links: HalLinks = Map.empty,
                           embedded: HalEmbeds = Map.empty
                         )(implicit valWriter: Writer[T]) extends HalObject {

  def withLink(key: String, link: HalLink): HalResource[T] = links.get(key) match {
    case None => copy(links = links + (key -> Right(link)))
    case Some(Right(existingLink)) => copy(links = links + (key -> Left(Seq(existingLink, link))))
    case Some(Left(existingLinks)) => copy(links = links + (key -> Left(existingLinks :+ link)))
  }

  def withLink[A](key: String, link: A)(implicit linker: HalLinkable[A]): HalResource[T] =
    withLink(key, linker.toHalLink(link))

  def withEmbedded(key: String, embed: HalResource[_]): HalResource[T] = embedded.get(key) match {
    case None => copy(embedded = embedded + (key -> Right(embed)))
    case Some(Right(existingEmbeds)) => copy(embedded = embedded + (key -> Left(Seq(existingEmbeds, embed))))
    case Some(Left(existingEmbeds)) => copy(embedded = embedded + (key -> Left(existingEmbeds :+ embed)))
  }

  def withEmbedded[A](key: String, obj: A)(implicit embed: HalEmbeddable[A]): HalResource[T] =
    withEmbedded(key, embed.toHalResource(obj))

  def toJson: JValue = {

    val jsonLinks: JValue =
      if(links.isEmpty)
        JNothing
      else
        links.map {
          case (key, Left(ls)) => (key, JArray(ls.map(l => l.toJson).toList))
          case (key, Right(ln)) => (key, ln.toJson)
        }

    val jsonEmbedded: JValue =
      if(embedded.isEmpty)
        JNothing
      else
        embedded.map {
          case (key, Left(es)) => (key, JArray(es.map(e => e.toJson).toList))
          case (key, Right(em)) => (key, em.toJson)
        }

    valWriter.write(value) merge ("_links" -> jsonLinks) ~ ("_embedded" -> jsonEmbedded)
  }
}

object HalResource {

  def apply(links: HalLinks, embedded: HalEmbeds) = HalResource[Unit](
    (),
    links,
    embedded
  )((obj: Unit) => JObject())

  type HalLinks = Map[String, Either[Seq[HalLink], HalLink]]
  type HalEmbeds = Map[String, Either[Seq[HalResource[_]], HalResource[_]]]

  def fromJson[T](json: JValue)(implicit extractor: HalExtractable[T]): HalResource[T] = {

    val links: Map[String, Either[Seq[HalLink], HalLink]] = json \ "_links" match {
      case JObject(linkSet) => linkSet.toMap.mapValues {
        case JArray(ls) => Left(ls.map(HalLink.fromJson))
        case obj => Right(HalLink.fromJson(obj))
      }
      case _ => Map.empty
    }

    val embedded: Map[String, Either[Seq[HalResource[_]], HalResource[_]]] = json \ "_embedded" match {
      case JObject(embeddedSet) => embeddedSet.map {
        case (key, JArray(embeds)) if extractor.extractors.contains(key) =>
          (key, Left(embeds.map(js => HalResource.fromJson(js)(extractor.extractors(key)))))
        case (key, obj) if extractor.extractors.contains(key) =>
          (key, Right(HalResource.fromJson(obj)(extractor.extractors(key))))
        case _ => (null, null)
      }.toMap
      case _ => Map.empty
    }

    val value = extractor.reader(links, embedded).read(json)

    HalResource(value, links, embedded)(extractor.writer)
  }

  implicit def writer[T]: Writer[HalResource[T]] = (obj: HalResource[T]) => obj.toJson

  implicit def resourceReserved[A <: HalObject](o: A): Either[Seq[A], A] = Right(o)
  implicit def resourceReserved[A <: HalObject](os: Seq[A]): Either[Seq[A], A] = Left(os)
  implicit def linkableReserved[A](a: A)(implicit linker: HalLinkable[A]): Either[Seq[HalLink], HalLink] = Right(linker.toHalLink(a))
  implicit def linkableReserved[A](as: Seq[A])(implicit linker: HalLinkable[A]): Either[Seq[HalLink], HalLink] = Left(as.map(linker.toHalLink))
  implicit def embeddableReserved[A](a: A)(implicit embeddable: HalEmbeddable[A]): Either[Seq[HalResource[A]], HalResource[A]] = Right(embeddable.toHalResource(a))
  implicit def embeddableReserved[A](as: Seq[A])(implicit embeddable: HalEmbeddable[A]): Either[Seq[HalResource[A]], HalResource[A]] = Left(as.map(embeddable.toHalResource))

}
