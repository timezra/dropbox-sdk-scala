package timezra.dropbox.core

object SprayJsonSupport extends SprayJsonSupport

trait SprayJsonSupport {
  import spray.json.jsonReader
  import spray.json.JsonParser
  import spray.json.RootJsonReader
  import spray.http.HttpCharsets.`UTF-8`
  import spray.http.HttpEntity.NonEmpty
  import spray.httpx.unmarshalling.Unmarshaller
  import ContentTypes.`text/javascript`

  implicit def sprayJsonUnmarshaller[T: RootJsonReader] = Unmarshaller[T](`text/javascript`) {
    case x: NonEmpty ⇒
      val json = JsonParser(x.asString(defaultCharset = `UTF-8`))
      jsonReader[T] read json
  }
}