package top

import java.io._
import java.util.Locale
import java.util.concurrent.TimeUnit

import com.amazonaws.services.lambda.runtime.Context
import play.api.libs.json.{JsObject, Json}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.matching.Regex
import scalaz.\/


/*
  Handles a request to check words for profanity. Retrieves the profanity regexes from its bucket,
  compiles it, and checks the passed words against the filter. Input & output are both JSON.

  Input:
  {
    "locale": "en",
    "terms": [
      "profane1",
      "profane2",
      ...
    ]
  }

  Output:
  {
    "result": [
      { "term": "profane1", "profane": true },
      { "term": "profane2", "profane": false },
      ...
    ]
  }

 */

/*

todo put your dependency .jar files in a separate /lib directory. This is faster than putting all your function’s code
in a single jar with a large number of .class files.

todo optimize memory settings
*/


object FilterRequest {

  case class InputTerms(locale: Option[String], terms: Seq[String])

  implicit val termsRead = Json.reads[InputTerms]

  def handler(in: InputStream, out: OutputStream, context: Context): Unit = {

    val logger = context.getLogger

    val input = parseInput(in)
    logger.log(s"Request: ${input.toString}")

    // todo AWS API Gateway doesn't support multi-valued query string parameters.
    // todo So this is problematic trying to support existing profanity filter interface.
    // Get the request body as InputTerms
    val terms = input.flatMap(getTerms)
    logger.log(s"Request terms: $terms")

    // Check each term against the regexes
    val tuples = for (ts <- terms) yield checkTerms(ts)

    // Create the response
    // Map this to Future[String] \/ Future[String], then merge into one Future[String]
    val fr = tuples.leftMap(t => {
      val error = t.toString
      Future(buildResponse(500, error))
    }).map(f => {
      f.map(seq => {
        // Construct JsArray from tuples
        val arr = Json.arr(seq.map { case (term, profane) => Json.obj("term" -> term, "profane" -> profane) })
        val body = Json.obj("result" -> arr)
        buildResponse(200, body)
      })
    }).merge

    val response = \/.fromTryCatchNonFatal(Await.result(fr, Duration(5, TimeUnit.SECONDS))).
      leftMap(t => buildResponse(500, t.toString)).merge

    withWriter(new OutputStreamWriter(out), writer => {
      logger.log(s"response: $response")
      writer.write(response)
    })
  }

  // The 'body' element is represented as a String, so it must be parsed first
  private def getTerms(obj: JsObject): Throwable \/ InputTerms =
    \/.fromTryCatchNonFatal(Json.parse((obj \ "body").as[String]).as[InputTerms])

  private def parseInput(in: InputStream): Throwable \/ JsObject =
    \/.fromTryCatchNonFatal(Json.parse(in).as[JsObject])


  /*
    Helpers to build the response.
    The response should look like this:
    {
      "statusCode": 200,
      "headers": {"Content-Type": "application/json"},
      "body": "body_text_goes_here"
    }
   */
  private def buildResponse(status: Int, body: JsObject): String =
    Json.stringify(Json.obj("statusCode" -> status, "body" -> Json.stringify(body)))

  private def buildResponse(status: Int, body: String): String =
    Json.stringify(Json.obj("statusCode" -> status, "body" -> body))


  // Check a single term against all regexes, stopping at the first match
  @tailrec
  private def checkTerm(term: String, locale: Locale, regexes: Seq[() => Regex]): (String, Boolean) = {
    if (regexes.isEmpty) term -> false else {
      if (regexes.head().findFirstMatchIn(term.toLowerCase(locale)).isDefined)
        term -> true else checkTerm(term, locale, regexes.tail)
    }
  }

  // Check each term in its own Future
  private def checkTerms(input: InputTerms): Future[Seq[(String, Boolean)]] = {
    val locale = new Locale(input.locale.getOrElse("en"))
    val support = RegexSupport(locale)
    val regexes = support.regexes.map(r => () => r.regex.r)
    val futures = input.terms.map(term => Future(checkTerm(term, locale, regexes)))
    Future.sequence(futures)
  }

  // Ensure the Writer gets closed
  private def withWriter[T <: Writer](writer: T, f: Writer => Unit): Unit = try {
    f(writer)
  } finally {
    writer.close()
  }

  // Testing
  def main(args: Array[String]): Unit = {
    val inputString =
      """{
        |    "body":"{\"terms\": [\"f_ck\", \"blart\", \"sh1t\", \"pus$i\"]}"
        |}
      """.stripMargin


    val is = new ByteArrayInputStream(inputString.getBytes)

    val r = for {
      ts <- parseInput(is).flatMap(getTerms)
      _ = println(ts)
    } yield checkTerms(ts)

    r.fold(t => {
      println(t)
    }, f => {
      val seq = Await.result(f, Duration.Inf)
      println(seq.mkString(", "))
    })
  }
}
