import akka.http.scaladsl.model.{MediaType, Uri}
import org.json4s.JsonAST.JString
import org.json4s.{JValue, JsonFormat}

package object hate {
  implicit object UriFormat extends JsonFormat[Uri] {
    def read(json: JValue) = json match { case JString(uri) => Uri(uri) }
    def write(obj: Uri) = JString(obj.toString)
  }

  implicit object MediaTypeFormat extends JsonFormat[MediaType] {
    def read(value: JValue) = value match { case JString(s) => MediaType.parse(s).right.get }
    def write(obj: MediaType) = JString(obj.toString)
  }

  object UriString {
    def unapply(arg: Uri): Option[String] = Some(arg.toString)
  }
}
