import akka.actor.Actor
import Util._
import DataTypes._
import scala.util.Random

/**
 * Created by ralphrecto on 12/4/16.
 */
class Client extends Actor {
  /** Client constants */
  // client id and version are used in generating peer ids
  private val CLIENT_ID = "RR"
  private val CLIENT_VERSION = "0001"

  // Client identifiers/helpers
  // TODO: this has to return a 20 byte string
  def genPeerId(): String = {
    s"-${CLIENT_ID}${CLIENT_VERSION}-${Random.nextInt(100000)}"
  }

  override def receive: Receive = ???

  def startTorrent(torrent : Torrent) : Unit = {
    torrent.info
  }

  def failTorrent(filename : String) : Unit = {
    Console.println(s"Oh no! ${filename} is not a valid torrent file.")
  }

  def main(args: Array[String]) : Unit = {
    val filename = args(0)
    val torrentSrc = io.Source.fromString(filename)
    val torrentStr = try torrentSrc.mkString finally torrentSrc.close()
    torrentStr |> Bencoding.decodeStr flatMap Torrent.beDecode match {
      case Some(torrent) => startTorrent(torrent)
      case None => failTorrent(filename)
    }
  }
}
