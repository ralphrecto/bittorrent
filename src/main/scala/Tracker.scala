import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

/**
 * Created by ralphrecto on 11/25/16.
 */
object Tracker extends App {

  // app globals/constants
  val DEFAULT_NUM_WANT = 50
  val host = "localhost"
  val port = 8080

  implicit val system = ActorSystem.create()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val route =
    path("") {
      get {
        parameters(
          'info_hash, 'peer_id, 'port.as[Int], 'uploaded.as[Int], 'downloaded.as[Int],
          'left.as[Int], 'compact.as[Int], 'no_peer_id, 'event, 'ip?,
          'numwant.as[Int] ? DEFAULT_NUM_WANT, 'key, 'trackerid
        ) {
          (infoHash, peerId, port, uploaded, downloaded, left, compact, noPeerId, event,
           ip, numWant, key, trackerId) => {
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "hello"))
          }
        }
      }
    }

  val bindingFuture = Http().bindAndHandle(route, host, port)

  Console.println(s"Running server at ${host}:${port}...")
}
