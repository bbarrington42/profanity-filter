package top

import java.io._
import java.util.Locale

import com.amazonaws.services.lambda.runtime.Context
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import play.api.libs.json.{JsObject, Json}

import scala.util.matching.Regex
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
    val response = Json.stringify(tuples.fold(t => {
      val error = t.getMessage
      logger.log(error)
      Json.obj("statusCode" -> 500, "body" -> error)
    }, items => {
      // Construct JsArray from tuples
      val arr = Json.arr(items.map { case (term, profane) => Json.obj("term" -> term, "profane" -> profane) })
      val body = Json.obj("result" -> arr)
      Json.obj("statusCode" -> 200, "body" -> Json.stringify(body))
    }))

    logger.log(s"response: $response")

    // ...and send it
    /*
    AWS documentation says the response should look like this:
    {
      "statusCode": 200,
      "headers": {"Content-Type": "application/json"},
      "body": "body_text_goes_here"
    }
     */

    withWriter(new OutputStreamWriter(out), w => {
      w.write(response)
    })

  }

  private def getBody(obj: JsObject): InputTerms = Json.parse((obj \ "body").as[String]).as[InputTerms]

  private def parseInput(in: InputStream): Throwable \/ JsObject =
    \/.fromTryCatchNonFatal(Json.parse(in).as[JsObject])


  private def getRegex: Throwable \/ Regex = {
    val s3 = AmazonS3ClientBuilder.defaultClient()
    \/.fromTryCatchNonFatal(s3.getObjectAsString(bucket, key).r)
  }

  private def checkTerms(input: InputTerms): Throwable \/ Seq[(String, Boolean)] = {
    // todo Ignore locale for now...
    val locale = Locale.getDefault
    getRegex.map(regex => {
      input.terms.map(w => w.toLowerCase(locale) match {
        case regex(_*) => (w, true)
        case _ => (w, false)
      })
    })
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
