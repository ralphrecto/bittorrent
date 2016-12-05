import akka.actor.Actor
import Util._

/**
 * Created by ralphrecto on 12/4/16.
 */
class Client extends Actor {
  override def receive: Receive = ???

  def startTorrent(torrent : Torrent) : Unit = {

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
