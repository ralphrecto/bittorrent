import scala.util.parsing.combinator._

object Bencoding {

  trait Bencodable {
    def beEncode(): BencodedExpr
  }

  /**
   * Abstract superclass for bencoded expressions
   * @param source the raw string for the expression
   */
  abstract class BencodedExpr(val source: String) extends Bencodable {
    def beEncode(): BencodedExpr = this
  }

  case class BeString(sourceArg: String, s: String) extends BencodedExpr(sourceArg)

  case class BeInt(sourceArg: String, i: Int) extends BencodedExpr(sourceArg)

  case class BeList(sourceArg: String, l: List[BencodedExpr]) extends BencodedExpr(sourceArg)

  case class BeDict(sourceArg: String, d: Map[String, BencodedExpr]) extends BencodedExpr(sourceArg)

  case class BeOpt(sourceArg: String, o: Option[BencodedExpr]) extends BencodedExpr(sourceArg)

  object BencodingParsers extends JavaTokenParsers {
    /**
     * Wrap a BencodingParser such that the source string is part of the result.
     * Note: why do this? This is more general than assuming that there is a bijection
     * between strings that are valid bencoded strings and BencodedExprs. I.e. if we were to
     * not care about whitespace when parsing, but still want to be able to have access
     * to the actual raw string given (e.g. for hashing). However, I'm not actually sure
     * if this generality is needed, but just to be safe we do it this way.
     *
     * @param beParser source parser
     * @return wrapped parser
     */
    def parserWrap(beParser: (String => Parser[BencodedExpr])): Parser[BencodedExpr] =
      new Parser[BencodedExpr] {
        override def apply(in: BencodingParsers.Input): BencodingParsers.ParseResult[BencodedExpr] = {
          beParser(in.source.toString).apply(in)
        }
      }

    def beExpr: Parser[BencodedExpr] = beInt | beString | beList | beDict

    def beInt: BencodingParsers.Parser[BencodedExpr] = {
      val rawParser = (source: String) => {
        "i" ~> wholeNumber <~ "e"
      } ^^ { (x) => BeInt(source, x.toInt) }
      parserWrap(rawParser)
    }

    def beString = {
      val rawParser = (source: String) => {
        wholeNumber <~ ":" ^^ (_.toInt)
      } >> {
        repN(_, """.""".r)
      } ^^ { (cl) => BeString(source, cl.reduce(_ + _)) }
      parserWrap(rawParser)
    }

    def beList = {
      val rawParser = (source: String) => {
        "l" ~> beExpr.* <~ "e"
      } ^^ { (parsedList) => BeList(source, parsedList) }
      parserWrap(rawParser)
    }

    def beDict = {
      val rawParser = (source: String) => {
        "d" ~> (beString ~ beExpr).* <~ "e"
      } ^^ { (kvPairs: List[BencodingParsers.~[BencodedExpr, BencodedExpr]]) =>
        val dictPairs = kvPairs map { (kvPair) => kvPair match {
          case BeString(_, key) ~ value => (key, value)
        }
        }
        BeDict(source, dictPairs.toMap)
      }
      parserWrap(rawParser)
    }

  }

  implicit class BencodableInt(i: Int) extends Bencodable {
    override def beEncode(): BencodedExpr = BeInt(s"i${i}e", i)
  }

  implicit class BencodableString(s: String) extends Bencodable {
    override def beEncode(): BencodedExpr = BeString(s"${s.length}:s", s)
  }

  implicit class BencodableBool(b: Boolean) extends Bencodable {
    override def beEncode(): BencodedExpr = (if (b) 1 else 0).beEncode()
  }

  implicit class BencodableList[Elt <: Bencodable](l: List[Elt]) extends Bencodable {
    override def beEncode(): BencodedExpr = {
      val encodedElts = l map (_.beEncode)
      val encodedStrs = encodedElts map (_.source)
      BeList(encodedStrs reduce (_ + _), encodedElts)
    }
  }

  implicit class BencodableMap[Value <: Bencodable](m: Map[String, Value]) extends Bencodable {
    override def beEncode(): BencodedExpr = {
      val encodedVals = m mapValues (_.beEncode)
      val eltSources = encodedVals map (pair => pair._1.beEncode.source + pair._2.source)
      BeDict(s"d${eltSources reduce (_ + _)}e", encodedVals)
    }
  }

  // TODO: why doesn't this generic version work? (i.e. if we remove bencodableOptionConv
  // we get compilation errors...)
  implicit def implicitOptionConv[A, B](from: Option[A])(implicit converter: A => B): Option[B] = {
    from map converter
  }

  implicit def bencodableOptionConv[A](from: Option[A])(implicit converter: A => Bencodable): Option[Bencodable] = {
    from map converter
  }

  def decodeStr(s: String): Option[BencodedExpr] = {
    val parseResult = BencodingParsers.parseAll(BencodingParsers.beExpr, s)
    if (parseResult.successful) Some(parseResult.get) else None
  }

  def decodeInt(e: BencodedExpr): Option[Int] = e match {
    case BeInt(_, i) => Some(i)
    case _ => None
  }

  def decodeString(e: BencodedExpr): Option[String] = e match {
    case BeString(_, s) => Some(s)
    case _ => None
  }

  def decodeList(e: BencodedExpr): Option[List[BencodedExpr]] = e match {
    case BeList(_, l) => Some(l)
    case _ => None
  }

}

