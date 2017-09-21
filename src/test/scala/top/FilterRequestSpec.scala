package top

import java.io.ByteArrayInputStream

import org.scalatest._
import top.FilterRequest._
import top.Regexes.ProfanityRegex

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class FilterRequestSpec extends FlatSpec with Matchers {
  val inputString =
    """{
      |    "body":"{\"terms\": [\"f_ck\", \"blart\", \"sh1t\", \"pus$i\"]}"
      |}
    """.stripMargin

  val regexes = Seq(ProfanityRegex(regex = """f.(c|k|c{1,})(c|k|k{1,})""", term = "fuck"))

  val is = new ByteArrayInputStream(inputString.getBytes)
  val input = parseInput(is)
  val terms = input.flatMap(getTerms)
  val profanity = terms.map(ts => checkTerms(ts, regexes))


  "The profanity filter request handler" should "correctly parse valid input" in {
    input.isRight should be(true)
  }

  it should "correctly extract the input terms" in {
    terms.isRight should be(true)
    terms.forall(in => in.locale.isEmpty && in.terms.nonEmpty) should be(true)
  }

  it should "correctly catch profane words" in {
    val f = profanity.getOrElse(Future(Seq.empty))
    val r = Await.result(f, Duration.Inf)

    r.nonEmpty should be (true)
  }
}
