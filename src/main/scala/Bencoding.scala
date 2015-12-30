import scala.util.parsing.combinator._

abstract class BencodedExpr
case class BeString(s: String) extends BencodedExpr
case class BeInt(i: Int) extends BencodedExpr
case class BeList(l: List[BencodedExpr]) extends BencodedExpr
case class BeDict(d: Map[BencodedExpr, BencodedExpr]) extends BencodedExpr

object BencodingParser extends {

}
