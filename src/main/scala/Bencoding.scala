import scala.util.parsing.combinator._

trait Bencodable {
  def beEncode() : BencodedExpr
}

abstract class BencodedExpr
case class BeString(s: String) extends BencodedExpr
case class BeInt(i: Int) extends BencodedExpr
case class BeList(l: List[BencodedExpr]) extends BencodedExpr
case class BeDict(d: Map[String, BencodedExpr]) extends BencodedExpr
case class BeOpt(o: Option[BencodedExpr]) extends BencodedExpr

object BencodingParser extends JavaTokenParsers {
  def beExpr : Parser[BencodedExpr] = beInt | beString | beList | beDict
  def beInt = { "i" ~> wholeNumber <~ "e" } ^^ { (x) => BeInt(x.toInt) }
  def beString = { wholeNumber <~ ":" ^^ (_.toInt) } >>
    { repN(_, """.""".r) } ^^
    { (cl) => BeString(cl.reduce(_+_)) }
  def beList = { "l" ~> beExpr.* <~ "e"} ^^ BeList
  def beDict = { "d" ~> (beString ~ beExpr).* <~ "e"} ^^ { (kvPairs) =>
    BeDict(kvPairs.map({ case BeString(s) ~ b => (s,b) }).toMap)
  }
}

object Bencoding {
  def decodeStr(s: String) : Option[BencodedExpr] = {
    val parseResult = BencodingParser.parseAll(BencodingParser.beExpr, s)
    if (parseResult.successful) Some(parseResult.get) else None
  }

  def encodeStr(e: BencodedExpr) : String = e match {
    case BeString(s) => s"${s.length}:$s"
    case BeInt(i) => s"i${i}e"
    case BeList(l) => s"l${l.foldLeft("")((acc, e) => acc + encodeStr(e))}e"
    case BeDict(d) => {
      val bencodeStrd = d.foldLeft("")((acc, kv) => {
        acc + encodeStr(BeString(kv._1)) + encodeStr(kv._2)
      })
      s"d${bencodeStrd}e"
    }
    case BeOpt(Some(e2)) => encodeStr(e2)
    case BeOpt(None) => ""
  }

  def encodeObj(o: Any) : BencodedExpr = o match {
    case s:String => BeString(s)
    case i:Int => BeInt(i)
    case l:List[Any] => BeList(l map encodeObj)
    case m:Map[String, Any] => BeDict(m mapValues encodeObj)
    case opt:Option[Any] => BeOpt(opt map encodeObj)
    case _ => BeOpt(None)
  }

  def decodeInt(e: BencodedExpr) : Option[Int] = e match {
    case BeInt(i) => Some(i)
    case _ => None
  }

  def decodeString(e: BencodedExpr) : Option[String] = e match {
    case BeString(s) => Some(s)
    case _ => None
  }

  def decodeList(e: BencodedExpr) : Option[List[BencodedExpr]] = e match {
    case BeList(l) => Some(l)
    case _ => None
  }

}

