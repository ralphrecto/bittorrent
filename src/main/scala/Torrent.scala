/* See https://wiki.theory.org/BitTorrentSpecification
 * for documentation of these types. */

import akka.util.ByteString

def beEnHelper(name: String, emain: Option[Any]): Option[(String, BencodedExpr)] = {
  def help(e: Any): BencodedExpr = e match {
    case i: Int => BeInt(i)
    case s: String => BeString(s)
    case l: List => BeList(l map help)
  }
  emain map (name -> help(_))
}

object MultiFileEntryInfo {
  def beDecode(be: BencodedExpr) : Option[MultiFileEntryInfo] = {
    for {
      BeDict(d) <- be
      length <- d get "length"
      lengthU <- Bencoding.unwrapInt(length)
      md5Sum <- d get "md5Sum"
      md5SumU <- Bencoding.unwrapString(md5Sum)
      path <- d get "path"
      pathU <- Bencoding.unwrapString(path)
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
      nameU <- Bencoding.unwrapString(name)
      length <- d get "length"
      lengthU <- Bencoding.unwrapInt(length)
    } yield {
      new SingleFileMoreInfo(
        nameU,
        lengthU,
        d get "md5Sum" flatMap Bencoding.unwrapString
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
      ("name" -> BeString(name)) +
      ("length" -> BeInt(length)) ++
      beEnHelper("md5Sum", md5Sum)
    )
  }
}

object MultiFileMoreInfo {
  def beDecode(d: Map[String, BencodedExpr]) : Option[MultiFileMoreInfo] = {
    for {
      name <- d get "name"
      nameU <- Bencoding.unwrapString(name)
      files <- d get "files"
      filesU <- Bencoding.unwrapList(files)
      filesU2 <- {
        val unwrapped = filesU map MultiFileEntryInfo.beDecode
        if (unwrapped forall (_.isDefined)) {
          Some(unwrapped map (_.get))
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
      pieceLengthU <- Bencoding.unwrapInt(pieceLength)
      pieces <- d get "pieces"
      piecesU <- Bencoding.unwrapString(pieces)
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
        d get "privateField" flatMap Bencoding.unwrapInt,
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
    val infoDict = Map[String, BencodedExpr]() +
      ("piece-length" -> BeInt(pieceLength)) +
      ("pieces" -> BeString(pieces.toString)) ++
      beEnHelper("private", privateField)
    val BeDict(moreDict) = moreInfo.beEncode
    BeDict(infoDict ++ moreDict)
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
      announceStr <- Bencoding.unwrapString(announce)
    } yield {
      new Torrent(
        infod.isMultifile(),
        infod,
        announceStr,
        for {
          topLevel <- d get "announceList"
          topLevelU <- Bencoding.unwrapList(topLevel)
          secondLevelU <- topLevelU match {
            case Nil => Some(Nil)
            case _ => {
              val unwrapped = topLevelU map Bencoding.unwrapList
              if (unwrapped forall (_.isDefined)) {
                Some(unwrapped map (_.get))
              } else None
            }
          }
          thirdLevelU <- {
            val unwrapped = secondLevelU map ((l) => {
              val unwrapped2 = l map Bencoding.unwrapString
              if (unwrapped2 forall (_.isDefined)) {
                Some(unwrapped2 map (_.get))
              } else None
            })
            if (unwrapped forall (_.isDefined)) {
              Some(unwrapped map (_.get))
            } else None
          }
        } yield thirdLevelU,
        d get "creation-date" flatMap Bencoding.unwrapInt,
        d get "comment" flatMap Bencoding.unwrapString,
        d get "created-by" flatMap Bencoding.unwrapString,
        d get "encoding" flatMap Bencoding.unwrapString
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
      ("announce" -> BeString(announce)) ++ {
        beEnHelper("announce-list", announceList) ++
        beEnHelper("creation-date", creationDate) ++
        beEnHelper("comment", comment) ++
        beEnHelper("created-by", createdBy) ++
        beEnHelper("encoding", encoding)
      }
    )
  }
}
