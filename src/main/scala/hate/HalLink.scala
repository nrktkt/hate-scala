package hate

import akka.http.scaladsl.model.{MediaType, Uri}
import org.json4s.JsonDSL._
import org.json4s._

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

  def toJson: JObject = {
    implicit val uriToJson: Uri => JValue = _.toString
    ("href" -> href.fold(identity, _.toString)) ~
      ("templated" -> Some(templated).filter(_ => templated)) ~
      ("type" -> `type`.map(_.toString)) ~
      ("deprecation" -> deprecation) ~
      ("name" -> name) ~
      ("profile" -> profile) ~
      ("title" -> title) ~
      ("hreflang" -> hreflang)
  }
}

object HalLink {
  implicit def templateableHref(template: String): Either[String, Uri] = Left(template)
  implicit def templateableHref(uri: Uri): Either[String, Uri] = Right(uri)

  implicit object HalLinkUri extends HalLinkable[Uri] {
    def toHalLink(obj: Uri): HalLink = HalLink(obj)
  }
}
