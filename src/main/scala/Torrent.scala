/* See https://wiki.theory.org/BitTorrentSpecification
 * for documentation of these types. */

import akka.util.ByteString

def beHelper(e: Any): BencodedExpr = e match {
  case i: Int => BeInt(i)
  case s: String => BeString(s)
  case l: List => BeList(l map beHelper)
}

class MultiFileEntryInfo(
  length: Int,
  md5Sum: String,
  path: String) extends Bencodable {

  def beEncode() : BencodedExpr = {
    BeDict(
      Map[String, BencodedExpr]() +
      ("length" -> BeInt(length)) +
      ("md5sum" -> BeString(md5Sum)) +
      ("path" -> BeString(path))
    )
  }
}

abstract class MoreInfo extends Bencodable

class SingleFileMoreInfo(
  name: String,
  length: Int,
  md5Sum: String) extends MoreInfo {

  def beEncode() : BencodedExpr = {
    BeDict(
      Map[String, BencodedExpr]() +
      ("name" -> BeString(name)) +
      ("length" -> BeInt(length)) +
      ("md5sum" -> BeString(md5Sum))
    )
  }
}

class MultiFileMoreInfo(
  name: String,
  files: List[MultiFileEntryInfo]) extends MoreInfo {

  def beEncode() : BencodedExpr = {
    BeDict(
      Map[String, BencodedExpr]() +
      ("name" -> BeString(name)) +
      ("files" -> BeList(files map (_.beEncode())))
    )
  }
}

object InfoDict {
  def beDecode(be: BencodedExpr): Option[InfoDict] = {
    for {
      BeDict(d) <- be
      pieceLength <- d get "piece-length"
      pieces <- d get "pieces"
    } yield {

    }
  }
}

abstract class InfoDict(
  pieceLength: Int,
  pieces: ByteString,
  privateField: Option[Int],
  moreInfo: MoreInfo) extends Bencodable {

  def beEncode() : BencodedExpr = {
    val infoDict = Map[String, BencodedExpr]() +
      ("piece-length" -> BeInt(pieceLength)) +
      ("pieces" -> BeString(pieces.toString)) ++ {
      privateField map ("private" -> beHelper(_))
    }
    val moreDict = moreInfo.beEncode match {
      case BeDict(d) => d
    }
    BeDict(infoDict ++ moreDict)
  }
}

object Torrent {
  def beDecode(be: BencodedExpr): Torrent = {
    for {
      BeDict(d) <- be
      info <- d get "info"
      announce <- d get "announce"
    } yield {

    }
  }
}

class Torrent(
  multiFile: Boolean,
  info: InfoDict,
  announce: String,
  announceList: Option[List[List[String]]],
  creationDate: Option[Int],
  comment: Option[String],
  createdBy: Option[String],
  encoding: Option[String]) extends Bencodable {

  def beEncode() : BencodedExpr = {
    BeDict(
      Map[String, BencodedExpr]() +
      ("info" -> info.beEncode()) +
      ("announce" -> BeString(announce)) ++ {
        (announceList map ("announce-list" -> beHelper(_))) ++
          (creationDate map ("creation-date" -> beHelper(_))) ++
          (comment map ("comment" -> beHelper(_))) ++
          (createdBy map ("created-by" -> beHelper(_))) ++
          (encoding map ("encoding" -> beHelper(_)))
      }
    )
  }
}
