/* See https://wiki.theory.org/BitTorrentSpecification
 * for documentation of these types. */

import akka.util.ByteString
import Bencoding.encodeObj

object MultiFileEntryInfo {
  def beDecode(be: BencodedExpr) : Option[MultiFileEntryInfo] = {
    for {
      BeDict(d) <- be
      length <- d get "length"
      lengthU <- Bencoding.decodeInt(length)
      md5Sum <- d get "md5Sum"
      md5SumU <- Bencoding.decodeString(md5Sum)
      path <- d get "path"
      pathU <- Bencoding.decodeString(path)
    } yield new MultiFileEntryInfo(lengthU, md5SumU, pathU)
  }
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

object SingleFileMoreInfo {
  def beDecode(d: Map[String,BencodedExpr]) : Option[SingleFileMoreInfo] = {
    for {
      name <- d get "name"
      nameU <- Bencoding.decodeString(name)
      length <- d get "length"
      lengthU <- Bencoding.decodeInt(length)
    } yield {
      new SingleFileMoreInfo(
        nameU,
        lengthU,
        d get "md5Sum" flatMap Bencoding.decodeString
      )
    }
  }
}

case class SingleFileMoreInfo(
  name: String,
  length: Int,
  md5Sum: Option[String]) extends MoreInfo {

  def beEncode() : BencodedExpr = {
    BeDict(
      Map[String, BencodedExpr]() +
      ("name" -> encodeObj(name)) +
      ("length" -> encodeObj(length)) +
      ("md5Sum" -> encodeObj(md5Sum))
    )
  }
}

object MultiFileMoreInfo {
  def beDecode(d: Map[String, BencodedExpr]) : Option[MultiFileMoreInfo] = {
    for {
      name <- d get "name"
      nameU <- Bencoding.decodeString(name)
      files <- d get "files"
      filesU <- Bencoding.decodeList(files)
      filesU2 <- {
        val decoded = filesU map MultiFileEntryInfo.beDecode
        if (decoded forall (_.isDefined)) {
          Some(decoded map (_.get))
        } else None
      }
    } yield new MultiFileMoreInfo(nameU, filesU2)
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
      pieceLengthU <- Bencoding.decodeInt(pieceLength)
      pieces <- d get "pieces"
      piecesU <- Bencoding.decodeString(pieces)
      singleFile <- d contains "length"
      moreInfo <- if (singleFile) {
        SingleFileMoreInfo.beDecode(d)
      } else {
        MultiFileMoreInfo.beDecode(d)
      }
    } yield {
      new InfoDict(
        pieceLengthU,
        ByteString.fromString(piecesU),
        d get "privateField" flatMap Bencoding.decodeInt,
        singleFile,
        moreInfo
      )
    }
  }
}

class InfoDict(
  pieceLength: Int,
  pieces: ByteString,
  privateField: Option[Int],
  multiFile: Boolean,
  moreInfo: MoreInfo) extends Bencodable {

  def beEncode() : BencodedExpr = {
    BeDict(
      Map[String, BencodedExpr]() +
      ("piece-length" -> BeInt(pieceLength)) +
      ("pieces" -> encodeObj(pieces.toString)) +
      ("private" -> encodeObj(privateField))
    )
  }

  def isMultifile() : Boolean = { this.multiFile }
}

object Torrent {
  def beDecode(be: BencodedExpr): Option[Torrent] = {
    for {
      BeDict(d) <- be
      info <- d get "info"
      infod <- InfoDict.beDecode(info)
      announce <- d get "announce"
      announceStr <- Bencoding.decodeString(announce)
    } yield {
      new Torrent(
        infod.isMultifile(),
        infod,
        announceStr,
        for {
          topLevel <- d get "announceList"
          topLevelU <- Bencoding.decodeList(topLevel)
          secondLevelU <- topLevelU match {
            case Nil => Some(Nil)
            case _ => {
              val decodeped = topLevelU map Bencoding.decodeList
              if (decodeped forall (_.isDefined)) {
                Some(decodeped map (_.get))
              } else None
            }
          }
          thirdLevelU <- {
            val decodeped = secondLevelU map ((l) => {
              val decodeped2 = l map Bencoding.decodeString
              if (decodeped2 forall (_.isDefined)) {
                Some(decodeped2 map (_.get))
              } else None
            })
            if (decodeped forall (_.isDefined)) {
              Some(decodeped map (_.get))
            } else None
          }
        } yield thirdLevelU,
        d get "creation-date" flatMap Bencoding.decodeInt,
        d get "comment" flatMap Bencoding.decodeString,
        d get "created-by" flatMap Bencoding.decodeString,
        d get "encoding" flatMap Bencoding.decodeString
      )
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
      ("announce" -> encodeObj(announce)) +
      ("announceList" -> encodeObj(announceList)) +
      ("creationDate" -> encodeObj(creationDate)) +
      ("comment" -> encodeObj(comment)) +
      ("createdBy" -> encodeObj(createdBy)) +
      ("encoding" -> encodeObj(encoding))
    )
  }
}
