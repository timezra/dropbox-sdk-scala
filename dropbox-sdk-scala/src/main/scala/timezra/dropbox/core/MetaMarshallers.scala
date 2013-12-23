package timezra.dropbox.core

import scalaz.effect.IO
import scalaz.effect.IoExceptionOr
import scalaz.iteratee.{ EnumeratorT ⇒ E }
import scalaz.iteratee.IterateeT.fold
import spray.http.HttpData
import spray.httpx.marshalling.LowerPriorityImplicitMetaMarshallers
import spray.httpx.marshalling.Marshaller

object MetaMarshallers extends MetaMarshallers

trait MetaMarshallers extends LowerPriorityImplicitMetaMarshallers {

  implicit def enumMarshaller[T <% HttpData] =
    Marshaller[E[IoExceptionOr[T], IO]] { (value, ctx) ⇒
      def handleError(t: Throwable): HttpData = {
        ctx handleError t
        HttpData.Empty
      }
      ctx.marshalTo(
        (fold[IoExceptionOr[T], IO, HttpData](HttpData.Empty)((d, e) ⇒ e.fold(handleError, d +: _)) &= value)
          .run
          .unsafePerformIO
      )
    }
}
