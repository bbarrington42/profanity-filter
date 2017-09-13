package top

import java.io._
import java.util.Locale

import com.amazonaws.services.lambda.runtime.Context
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.matching.Regex
import scala.util.{Failure, Success}
import scalaz.\/


/*
  Handles a request to check words for profanity. Retrieves the profanity regex from its bucket,
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

  // todo Bucket & key should be provided via the environment. For now just hard-code
  val bucket = "ccfs-profanity-regex"
  val key = "regex.txt"


  def handler(in: InputStream, out: OutputStream, context: Context): Unit = {

    val logger = context.getLogger

    val input = parseInput(in)
    logger.log(s"Request: ${input.toString}")

    // todo AWS API Gateway doesn't support multi-valued query string parameters.
    // todo So this is problematic trying to support existing profanity filter interface.
    // Get the request body as InputTerms
    val body = input.flatMap(obj => \/.fromTryCatchNonFatal(getBody(obj)))
    logger.log(s"Request body: $body")

    // Check each term against the regex
    val tuples = body.flatMap(checkTerms)

    // Create the response
    // Map this to String \/ Future[String] first, then write the output
    val response = tuples.leftMap(t => {
      val error = t.toString
      logger.log(error)
      buildResponse(500, error)
    }).map(f => {
      f.map(seq => {
        // Construct JsArray from tuples
        val arr = Json.arr(seq.map { case (term, profane) => Json.obj("term" -> term, "profane" -> profane) })
        val body = Json.obj("result" -> arr)
        logger.log(body.toString)
        buildResponse(200, body)
      })
    })

    withWriter(new OutputStreamWriter(out), writer => {
      response.fold(r => writer.write(r),
        fr => {
          fr.onComplete {
            case Success(r) => writer.write(r)
            case Failure(e) => writer.write(buildResponse(500, e.toString))
          }
        })
    })
  }

  private def getBody(obj: JsObject): InputTerms =
  // The 'body' element is represented as a String, so it must be parsed first
    Json.parse((obj \ "body").as[String]).as[InputTerms]

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


  /*
    S3 object will have the following format:
    '{
      "regexes": [
        "<regex1>",
        "<regex2>,
        ...
        ]
     }'
   */
  // Modify this to return a List of Futures. Return the result of the first failing or success if no failures. (?)
  private def getRegexes: Throwable \/ List[Regex] = {
    val s3 = AmazonS3ClientBuilder.defaultClient()
    \/.fromTryCatchNonFatal {
      val json = s3.getObjectAsString(bucket, key)
      println(s"Regex json: $json")
      val arr = (Json.parse(json).as[JsObject] \ "regexes").as[JsArray]
      arr.value.map(_.as[String].r).toList
    }
  }

  @tailrec
  private def checkTerm(term: String, locale: Locale, regexes: List[Regex]): (String, Boolean) = regexes match {
    case Nil => (term, false)
    case head :: tail =>
      if (head.findFirstMatchIn(term.toLowerCase(locale)).isDefined)
        (term, true) else checkTerm(term, locale, tail)
  }

  private def checkTerms(input: InputTerms): Throwable \/ Future[Seq[(String, Boolean)]] = {
    val locale = new Locale(input.locale.getOrElse("en"))
    val futures = getRegexes.map(regexes => input.terms.map(term => Future(checkTerm(term, locale, regexes))))
    futures.map(seq => Future.sequence(seq))
  }

  private def withWriter[T <: Writer](writer: T, f: Writer => Unit): Unit = try {
    f(writer)
  } finally {
    writer.close()
  }

  // Testing
  def main(args: Array[String]): Unit = {
    val inputString =
      """{
        |    "body":"{\n    \"locale\": \"en\",\n    \"terms\": [\n        \"blart\",\n        \"pussy\",\n        \"shiiit\",\n        \"f_ck\"\n  ]}"
        |}
      """.stripMargin


    val is = new ByteArrayInputStream(inputString.getBytes)
    val result = parseInput(is)

    println(s"result: $result")

    val body = result.flatMap(obj => \/.fromTryCatchNonFatal(getBody(obj)))

    println(s"body: $body")
  }
}
