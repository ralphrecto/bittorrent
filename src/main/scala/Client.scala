import akka.actor.Actor
import Util._
import DataTypes._
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model._
import akka.stream.{ActorMaterializerSettings, ActorMaterializer}
import scala.util.Random

object Client {
  private final val CLIENT_ID = "RR"
  private final val CLIENT_VERSION = "0001"

  def main(args: Array[String]): Unit = {
    val client = new Client()
    val filename = args(0)
    val torrentSrc = io.Source.fromString(filename)
    val torrentStr = try torrentSrc.mkString finally torrentSrc.close()
    torrentStr |> Bencoding.decodeStr flatMap Torrent.beDecode match {
      case Some(torrent: Torrent) => client.startTorrent(torrent)
      case None => client.failTorrent(filename)
    }
  }
}

/**
 * An enum for the event param in tracker HTTP requests
 * @param name string of the enum member
 */
sealed abstract class EventParam(val name: String) {
  override def toString = name
}

case class Started() extends EventParam("started")

case class Stopped() extends EventParam("stopped")

case class Completed() extends EventParam("completed")

/**
 * Utility class for holding all client state for the current torrent
 *
 * @param torrent metadata about torrent being downloaded
 * @param uploaded # of bytes uploaded
 * @param downloaded # of bytes already downloaded
 * @param left # of bytes left to download
 * @param peerId id for the given client instance
 * @param port where the client is listening for peer connections
 */
final class ClientState(val torrent: Torrent, val uploaded: Int, val downloaded: Int, val left: Int, val peerId: String, val port: Int)

/**
 * Created by ralphrecto on 12/4/16.
 */
class Client extends Actor {

  import akka.pattern.pipe
  import context.dispatcher

  final implicit val materializer: ActorMaterializer = ActorMaterializer(
    ActorMaterializerSettings(context.system)
  )

  private val http = Http(context.system)

  private val peerId = genPeerId()
  private val listeningPort = genPort()

  // TODO: this has to return a 20 byte string
  // TODO: the random component should probs incorporate pid, timestart, etc.
  def genPeerId(): String = {
    s"-${Client.CLIENT_ID}${Client.CLIENT_VERSION}-${Random.nextInt(100000)}"
  }

  // TODO: make this actually listen on the port; in range 6881-6889
  def genPort(): Int = {
    return 6881
  }

  override def receive: Receive = {
    case HttpResponse(StatusCodes.OK, headers, entity, _) => {
      Console.print("great success!")
    }
    case HttpResponse(_, _, _, _) => {
      Console.print("not a great success!")
    }
  }

  /**
   * Form the initial tracker request to begin the torrent
   * @param state client state for the current download
   * @param event value for the event param (see EventParam class)
   */
  def createTrackerRequest(state: ClientState, event: EventParam): HttpRequest = {
    val baseUri = state.torrent.announce
    val params: Map[String, String] = Map[String, String]() +
      ("info_hash" -> (state.torrent.info.beEncode().source |> md5Digest |> urlEncode)) +
      ("peer_id" -> peerId) +
      ("port" -> listeningPort.toString) +
      ("uploaded" -> state.uploaded.toString) +
      ("downloaded" -> state.downloaded.toString) +
      ("left" -> state.left.toString) +
      ("compact" -> 1.toString) +
      ("event" -> event.toString)

    HttpRequest(
      HttpMethods.GET,
      uri = Uri(baseUri).withQuery(Query(params))
    )
  }

  /**
   * This function should encompass the different stages of beginning a torrent
   * 1. Setup (create setup state, begin listening on a port, etc.)
   * 2. Send HTTP request to tracker
   * 3. Begin downloading from peers
   *
   * @param torrent metadata regarding the file/s to be downloaded
   */
  def startTorrent(torrent: Torrent): Unit = {
    // 1. setup: initialize state, setup ports
    // TODO: calc left bytes properly
    val left = 100
    val initState = new ClientState(torrent, 0, 0, left, genPeerId(), genPort())

    // 2. send request to tracker
    val initRequest = createTrackerRequest(initState, Started())
    http.singleRequest(initRequest).pipeTo(self)
    ()
  }

  def failTorrent(filename: String): Unit = {
    Console.println(s"Oh no! ${filename} is not a valid torrent file.")
  }
}
