import akka.actor.{Props, ActorSystem, Actor}
import Util._
import DataTypes._
import akka.http.scaladsl.{HttpExt, Http}
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model._
import akka.stream.{ActorMaterializerSettings, ActorMaterializer}
import scala.util.Random

object Client {
  private final val CLIENT_ID = "RR"
  private final val CLIENT_VERSION = "0001"

  abstract class ClientMessage

  case class Setup(torrent: Torrent)

  case class RequestResponse(response: HttpResponse) extends ClientMessage

  def failTorrent(filename: String): Unit = {
    Console.println(s"${filename} is not a valid torrent file.")
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("BittorrentSystem")
    val client = system.actorOf(Props[Client], "BittorrentClient")

    val filename = args(0)
    val torrentSrc = io.Source.fromString(filename)
    val torrentStr = try torrentSrc.mkString finally torrentSrc.close()
    torrentStr |> Bencoding.decodeStr flatMap Torrent.beDecode match {
      case Some(torrent: Torrent) => client ! Setup(torrent)
      case None => failTorrent(filename)
    }

    system.terminate()
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
 * Class for encapsulating actor state
 */
sealed abstract class ClientState

case class Unintiated() extends ClientState

/**
 * State representing an initiated torrent
 *
 * @param torrent metadata about torrent being downloaded
 * @param uploaded # of bytes uploaded
 * @param downloaded # of bytes already downloaded
 * @param left # of bytes left to download
 * @param peerId id for the given client instance
 * @param port where the client is listening for peer connections
 */
case class Initiated(torrent: Torrent, uploaded: Int, downloaded: Int, left: Int, peerId: String, port: Int) extends ClientState

/**
 * Created by ralphrecto on 12/4/16.
 */
class Client() extends Actor {

  import Client._
  import akka.pattern.pipe
  import context.dispatcher

  final implicit val materializer: ActorMaterializer = ActorMaterializer(
    ActorMaterializerSettings(context.system)
  )

  private val http: HttpExt = Http(context.system)

  private var currentState: ClientState = Unintiated()

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
    // receive the torrent metadata, initialize torrent download
    case Setup(torrent) => {
      // 1. setup: initialize state, setup ports
      // TODO: calc left bytes properly
      val left = 100
      val initState: Initiated = Initiated(torrent, 0, 0, left, genPeerId(), genPort())
      currentState = initState

      // 2. send request to tracker
      val initRequest = createTrackerRequest(initState, Started())
      http.singleRequest(initRequest).map(RequestResponse(_)).pipeTo(self)
    }

    case RequestResponse(resp) => resp match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => {
        Console.print("great success!")
      }
      case HttpResponse(_, _, _, _) => {
        Console.print("not a great success!")
      }
    }
  }

  /**
   * Form the initial tracker request to begin the torrent
   * @param state client state for the current download
   * @param event value for the event param (see EventParam class)
   */
  def createTrackerRequest(state: Initiated, event: EventParam): HttpRequest = {
    val baseUri = state.torrent.announce
    val params: Map[String, String] = Map[String, String]() +
      ("info_hash" -> (state.torrent.info.beEncode().source |> md5Digest |> urlEncode)) +
      ("peer_id" -> state.peerId) +
      ("port" -> state.port.toString) +
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

}
