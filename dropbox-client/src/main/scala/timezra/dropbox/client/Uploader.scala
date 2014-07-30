package timezra.dropbox.client

import com.typesafe.config.ConfigFactory
import timezra.dropbox.core.Dropbox
import timezra.dropbox.core.Implicits
import java.io.File

class Uploader(dropbox: Dropbox) {

  def putDirectory(root: File, path: String = "/", limit: Int = Integer.MAX_VALUE): Unit = {
     new Files(root) take(limit)
//    for (f <- new Files(root)) {}
//    processDirectory(root, path)

    def processDirectory(dir: File, parentPath: String): Unit = {
      val path = s"${parentPath}/${dir.getName}"
      val files = dir.listFiles.map(f ⇒ f.getCanonicalFile())
      for (f ← files) {
        if (f.isDirectory) {
          processDirectory(f, path)
        } else {
          val filePath = s"${path}/${f.getName}"
          import Implicits._
          dropbox putFile (path = filePath, length = f.length().intValue(), contents = f)
        }
      }
    }
  }

  class Files(root: File, path: String = "/") extends Iterable[Unit] {

    override def iterator(): Iterator[Unit] = {
      return new Iterator[Unit]() {

        var cont: (Unit ⇒ Unit) = null

        import scala.util.continuations._
        def processDirectory(dir: File, parentPath: String): Unit @cps[Unit] = {
          val path = s"${parentPath}/${dir.getName}"
          val files = dir.listFiles.map(f ⇒ f.getCanonicalFile())
          var i = 0
          while (i < files.length) {
            val f = files(i);
            i += 1
            if (f.isDirectory) {
              processDirectory(f, path)
            } else {
              shift {
                k: (Unit ⇒ Unit) ⇒
                  {
                    cont = k
                  }
              }
              import Implicits._
              val filePath = s"${path}/${f.getName}"
              dropbox putFile (path = filePath, length = f.length().intValue(), contents = f)
            }
          }
          cont = null
        }

        reset {
          processDirectory(root, path)
        }

        def hasNext: Boolean = cont != null
        def next(): Unit = cont()
      }
    }
  }
}

object Uploader {
  val config = ConfigFactory.load().getConfig("timezra.dropbox.client").getConfig("test").getConfig("client")
  val clientIdentifier = config.getString("clientIdentifier")
  val accessToken = config.getString("accessToken")

  def apply(): Uploader = new Uploader(Dropbox(clientIdentifier, accessToken))

  def main(args: Array[String]) {
    Uploader().putDirectory(new File("src/test/resources/folder1"))
  }
}