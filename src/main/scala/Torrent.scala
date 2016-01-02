/* See https://wiki.theory.org/BitTorrentSpecification
 * for documentation of these types. */

import akka.util.ByteString

class MultiFileEntryInfo(
  length: Int,
  md5Sum: String,
  path: String) extends Bencodable {

  def bencode() : BencodedExpr = {
    val entryDict = Map[String, BencodedExpr]() +
      ("length" -> BeInt(length)) +
      ("md5sum" -> BeString(md5Sum)) +
      ("path" -> BeString(path))
    BeDict(entryDict)
  }
}

abstract class MoreInfo extends Bencodable

class SingleFileMoreInfo(
  name: String,
  length: Int,
  md5Sum: String) extends MoreInfo {

  def bencode() : BencodedExpr = {
    val moreDict = Map[String, BencodedExpr]() +
      ("name" -> BeString(name)) +
      ("length" -> BeInt(length)) +
      ("md5sum" -> BeString(md5Sum))
    BeDict(moreDict)
  }
}

class MultiFileMoreInfo(
  name: String,
  files: List[MultiFileEntryInfo]) extends MoreInfo {

  def bencode() : BencodedExpr = {
    val moreDict = Map[String, BencodedExpr]() +
      ("name" -> BeString(name)) +
      ("files" -> BeList(files map (_.bencode())))
    BeDict(moreDict)
  }
}

abstract class InfoDict(
  pieceLength: Int,
  pieces: ByteString,
  privateField: Option[Int],
  moreInfo: MoreInfo) extends Bencodable {

  def bencode() : BencodedExpr = {
    var infoDict = Map[String, BencodedExpr]() +
      ("piece-length" -> BeInt(pieceLength)) +
      ("pieces" -> BeString(pieces.toString))
    if (privateField.isDefined) {
      infoDict += ("private" -> BeInt(privateField.get))
    }
    val moreDict = moreInfo.bencode match {
      case BeDict(d) => d
    }
    BeDict(infoDict ++ moreDict)
  }
}

case class Torrent(
  multiFile: Boolean,
  info: InfoDict,
  announce: String,
  announceList: Option[List[List[String]]],
  creationDate: Option[Int],
  comment: Option[String],
  createdBy: Option[String],
  encoding: Option[String]) extends Bencodable {

  def bencode() : BencodedExpr = {
    var torrentDict = Map[String, BencodedExpr]() +
      ("info" -> info.bencode()) +
      ("announce" -> BeString(announce))
    if (announceList.isDefined) {
      torrentDict += ("announce-list" ->
        BeList(announceList.get map { (l) => BeList(l map BeString) })
      )
    }
    if (creationDate.isDefined) {
      torrentDict += ("creation-date" -> BeInt(creationDate.get))
    }
    if (comment.isDefined) {
      torrentDict += ("comment" -> BeString(comment.get))
    }
    if (createdBy.isDefined) {
      torrentDict += ("created-by" -> BeString(createdBy.get))
    }
    if (encoding.isDefined) {
      torrentDict += ("encoding" -> BeString(encoding.get))
    }
    BeDict(torrentDict)
  }
}
