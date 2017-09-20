package top

import java.io.ByteArrayInputStream

import org.scalatest._
import play.api.libs.json.JsArray
import top.FilterRequest._

class FilterRequestSpec extends FlatSpec with Matchers {

  "The profanity filter request handler" should "correctly parse valid input" in {
    val inputString =
      """{
        |    "body":"{\"terms\": [\"f_ck\", \"blart\", \"sh1t\", \"pus$i\"]}"
        |}
      """.stripMargin

    val is = new ByteArrayInputStream(inputString.getBytes)
    val input = parseInput(is)

    input.isRight should be (true)
  }
}
