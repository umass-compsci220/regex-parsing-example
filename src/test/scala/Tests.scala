import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object GenRegex {

  def genChar: Gen[Char] = Gen.oneOf('a', 'b', 'c')

  def genRChar: Gen[Regex] = for {
    ch <- genChar
  } yield (RChar(ch))

  def genRAlt(size: Int): Gen[Regex] = for {
    r1 <- genRegex(size / 2)
    r2 <- genRegex(size / 2)
  } yield (RAlt(r1, r2))

  def genRSeq(size: Int): Gen[Regex] = for {
    r1 <- genRegex(size / 2)
    r2 <- genRegex(size / 2)
  } yield (RSeq(r1, r2))

  def genRegex(size: Int): Gen[Regex] = {
    if (size <= 0) genRChar
    else Gen.oneOf(genRAlt(size), genRSeq(size), genRChar)
  }

  def regex: Gen[Regex] = Gen.sized(genRegex)
}

class TestSuite extends org.scalatest.FunSuite with GeneratorDrivenPropertyChecks {

  def check1(str: String) = {
    val re1 = Parser.parseAll(Parser.regex, str).get
    val re2 = Parser.parseAll(Parser.regex, Pretty.print(re1)).get
    assert(re1 == re2)
  }

 def check2(re1: Regex) = {
    val re2 = Parser.parseAll(Parser.regex, Pretty.print(re1)).get
    re1 == re2
  }

  test("test 1") {
    forAll(Gen.sized(GenRegex.genRegex)) { re =>
      println(re)
      check2(re)
    }
  }
}