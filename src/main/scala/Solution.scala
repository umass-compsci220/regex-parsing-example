import scala.util.parsing.combinator._

sealed trait Regex
case class RChar(ch: Char) extends Regex
case class RSeq(s1: Regex, s2: Regex) extends Regex
case class RAlt(s1: Regex, s2: Regex) extends Regex

object Parser extends RegexParsers with PackratParsers {
  // <char> ::= "a" | "b" | "c"
  lazy val char: PackratParser[String] = "a" | "b" | "c"

  // <aregex> ::= <char>
  //           | <aregex> "|" <aregex>
  //           | "(" <regex> ")"
  lazy val aregex: PackratParser[Regex] = (
    aregex ~ "|" ~ aregex ^^ { case r1 ~ _ ~ r2 => RAlt(r1, r2) } |
    char ^^ { case ch => RChar(ch.charAt(0)) } |
    "(" ~ regex ~ ")" ^^ { case _ ~ r ~ _ => r }
  )

  // <regex> ::= <aregex>
  //           | <regex> <regex>
  lazy val regex: PackratParser[Regex] = (
    regex ~ regex ^^ { case r1 ~ r2 => RSeq(r1, r2) } |
    aregex ^^ { case r => r }
  )
}

object Pretty {

  def print(re: Regex): String = re match {
    case RChar(ch) => ch.toString
    case RSeq(r1, r2) => s"(${print(r1)}${print(r2)})"
    case RAlt(r1, r2) => s"(${print(r1)}|${print(r2)})"
  }
}
