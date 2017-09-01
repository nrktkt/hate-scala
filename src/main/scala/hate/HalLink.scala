package hate

import akka.http.scaladsl.model.{MediaType, Uri}
import org.json4s
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.DefaultReaders._

case class HalLink(
                    href: Either[String, Uri],
                    `type`: Option[MediaType] = None,
                    deprecation: Option[Uri] = None,
                    name: Option[String] = None,
                    profile: Option[Uri] = None,
                    title: Option[String] = None,
                    hreflang: Option[String] = None
                  ) extends HalObject {
  val templated = href.isLeft

  def toJson(implicit writer: Writer[HalLink]) = writer.write(this)
}

object HalLink {

  object HalHref {
    def unapply(arg: HalLink): Option[Either[String, Uri]] = Some(arg.href)
  }

  implicit class JsOptPaths(val json: JValue) extends AnyVal {
    def \[T <: JValue](field: String, clazz: Class[T]): Option[T] = json \ field match {
      case js if clazz isAssignableFrom js.getClass => Some(js.asInstanceOf[T])
      case _ => None
    }
  }

  implicit def format(implicit uriForm: JsonFormat[Uri], mediaTypeForm: JsonFormat[Option[MediaType]]) = new JsonFormat[HalLink] {
    def read(json: json4s.JValue) = {
      val templated = (json \ ("templated", classOf[JBool])).exists(_.value)
      val href = json \ "href"
      val typ = mediaTypeForm.read(json \ "type")
      val deprecation = OptionReader(uriForm).read(json \ "deprecation")
      val name = (json \ ("name", classOf[JString])).map(_.s)
      val profile = OptionReader(uriForm).read(json \ "profile")
      val title = (json \ ("title", classOf[JString])).map(_.s)
      val hreflang = (json \ ("hreflang", classOf[JString])).map(_.s)

      HalLink(
        if (templated) href match {
          case JString(value) => value
        } else uriForm.read(href),
        typ,
        deprecation,
        name,
        profile,
        title,
        hreflang
      )
    }

    implicit def writeable2JValue[T](v: T)(implicit writer: Writer[T]): json4s.JValue = writer.write(v)

    def write(obj: HalLink): json4s.JValue = {
      ("href" -> obj.href.fold(identity, _.toString)) ~
        ("templated" -> Some(obj.templated).filter(identity)) ~
        ("type" -> obj.`type`) ~
        ("deprecation" -> obj.deprecation) ~
        ("name" -> obj.name) ~
        ("profile" -> obj.profile) ~
        ("title" -> obj.title) ~
        ("hreflang" -> obj.hreflang)
    }.noNulls
  }

  def fromJson(json: JValue)(implicit reader: Reader[HalLink]): HalLink = reader.read(json)

  implicit def templateableHref(template: String): Either[String, Uri] = Left(template)
  implicit def templateableHref(uri: Uri): Either[String, Uri] = Right(uri)

  implicit object HalLinkUri extends HalLinkable[Uri] {
    def toHalLink(obj: Uri): HalLink = HalLink(obj)
  }
}
