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
  def decode(s: String) : BencodedExpr = {
    BencodingParser.parseAll(BencodingParser.beExpr, s).get
  }

  def encode(e: BencodedExpr) : String = e match {
    case BeString(s) => s"${s.length}:$s"
    case BeInt(i) => s"i${i}e"
    case BeList(l) => s"l${l.foldLeft("")((acc, e) => acc + encode(e))}e"
    case BeDict(d) => {
      val bencoded = d.foldLeft("")((acc, kv) => {
        acc + encode(BeString(kv._1)) + encode(kv._2)
      })
      s"d${bencoded}e"
    }
    case BeOpt(Some(e2)) => encode(e2)
    case BeOpt(None) => ""
  }

  def unwrapInt(e: BencodedExpr) : Option[Int] = e match {
    case BeInt(i) => Some(i)
      _ => None
  }

  def unwrapString(e: BencodedExpr) : Option[String] = e match {
    case BeString(s) => Some(s)
      _ => None
  }

  def unwrapList(e: BencodedExpr) : Option[List[BencodedExpr]] = e match {
    case BeList(l) => Some(l)
      _ => None
  }

  def main(args: Array[String]): Unit = {
    val s = "d2:hili10ei100e5:helloee"
    println(decode(s))
    println(encode(decode(s)) equals s)
  }

}

