package top

import java.io._

import com.amazonaws.services.lambda.runtime.Context
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import play.api.libs.json.{JsObject, Json}


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

/* todo Consider a TTL for the regex?  Maybe just a timestamp check on when the regex was last modified

todo put your dependency .jar files in a separate /lib directory. This is faster than putting all your function’s code
in a single jar with a large number of .class files.

todo optimize memory settings
*/


object FilterRequest {

  private val s3 = AmazonS3ClientBuilder.defaultClient()

  def handler(in: InputStream, out: OutputStream, context: Context): Unit = {
    // todo Ensure words are transformed to lower case before checking for profanity
    val logger = context.getLogger

    // todo Testing
    val input = Json.parse(in).asOpt[JsObject].getOrElse(JsObject.empty)
    logger.log(input.toString)

    /*
    AWS documentation says the response should look like this:
    {
      "statusCode": 200,
      "headers": {"Content-Type": "application/json"},
      "body": "body_text_goes_here"
    }
     */

    val writer = new OutputStreamWriter(out)

    withWriter(writer, w => {

      val body = Json.obj("field" -> "blah")
      val headers = Json.obj("Content-Type" -> "application/json")
      val output = Json.obj("statusCode" -> 200, "isBase64Encoded" -> false,
        "headers" -> headers, "body" -> Json.stringify(body))

      val response = Json.stringify(output)
      logger.log(s"response: $response")

      w.write(response)
    })

  }

  def withWriter[T <: Writer](writer: T, f: Writer => Unit): Unit = try {
    f(writer)
  } finally {
    writer.close()
  }
}

/*

  // Checks the passed list of words against the filter and returns the passed word with a Boolean flag: true if it is
  // profane, false if not.
  def profanityCheck(words: List[String]): List[(String, Boolean)] = {
    words.map(w => w.toLowerCase(locale) match {
      case profanityRegex(_*) => (w, true)
      case _ => (w, false)
    })
  }
 */
