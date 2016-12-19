/* See https://wiki.theory.org/BitTorrentSpecification
 * for documentation of these types. */

import akka.util.ByteString
import _root_.Bencoding._
import Util._

def addIfPresent[T <: Bencodable](kv: (String, Option[T]))(m: Map[String, Bencodable]) = kv match {
  case (key, Some(value)) => m + (key -> value)
  case (_, None) => m
}

object MultiFileEntryInfo {
  def beDecode(be: BencodedExpr) : Option[MultiFileEntryInfo] = {
    val BeDict(_, d) = be
    for {
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
    val l:List[(String, Bencodable)] = List(
      ("length" -> length),
      ("md5Sum" -> md5Sum),
      ("path" -> path)
    )

    l.toMap.beEncode()
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
    val l:List[(String, Bencodable)] = List(
      ("name" -> name),
      ("length" -> length)
    )

    addIfPresent("md5Sum" -> md5Sum)(l.toMap).beEncode()
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
    val l:List[(String, Bencodable)] = List(
      ("name" -> name),
      ("files" -> files)
    )

    l.toMap.beEncode()
  }
}

object InfoDict {
  def beDecode(be: BencodedExpr): Option[InfoDict] = {
    val BeDict(_, d) = be
    for {
      pieceLength <- d get "piece-length"
      pieceLengthU <- Bencoding.decodeInt(pieceLength)
      pieces <- d get "pieces"
      piecesU <- Bencoding.decodeString(pieces)
      // length is a required field for single file infos
      moreInfo <- if (d contains "length") {
        SingleFileMoreInfo.beDecode(d)
      } else {
        MultiFileMoreInfo.beDecode(d)
      }
    } yield {
      new InfoDict(
        pieceLengthU,
        ByteString.fromString(piecesU),
        d contains "length",
        moreInfo,
        d.get("privateField") flatMap Bencoding.decodeInt,
        be
      )
    }
  }
}

class InfoDict(
  val pieceLength: Int,
  val pieces: ByteString,
  val multiFile: Boolean,
  val moreInfo: MoreInfo,
  val privateField: Option[Int],
  // the original bencoded expr
  val original: BencodedExpr) extends Bencodable {

  def beEncode() : BencodedExpr = {
    val l:List[(String, Bencodable)] = List(
      ("piece-length" -> pieceLength),
      ("pieces" -> pieces.toString),
      ("multiFile" -> multiFile),
      ("moreInfo" -> moreInfo)
    )

    addIfPresent("privateField" -> privateField)(l.toMap).beEncode()
  }
}

object Torrent {
  def beDecode(be: BencodedExpr): Option[Torrent] = {
    val BeDict(_, d) = be
    for {
      info <- d get "info"
      infod <- InfoDict.beDecode(info)
      announce <- d get "announce"
      announceStr <- Bencoding.decodeString(announce)
    } yield {
      new Torrent(
        infod.multiFile,
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
  val multiFile: Boolean,
  val info: InfoDict,
  val announce: String,
  val announceList: Option[List[List[String]]],
  val creationDate: Option[Int],
  val comment: Option[String],
  val createdBy: Option[String],
  val encoding: Option[String]) extends Bencodable {

  def beEncode() : BencodedExpr = {
    val dict: Map[String, Bencodable] =
      Map[String, Bencodable]() +
      ("info" -> info) +
      ("announce" -> announce)

    val finalDict: Map[String, Bencodable] =
      dict |>
      addIfPresent("announceList" -> announceList) |>
      addIfPresent("creationDate" -> creationDate) |>
      addIfPresent("comment" -> comment) |>
      addIfPresent("createdBy" -> createdBy) |>
      addIfPresent("encoding" -> encoding)

    finalDict.beEncode()
  }
}
