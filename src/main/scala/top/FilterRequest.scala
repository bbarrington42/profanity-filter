package top

import java.io.{InputStream, OutputStream, OutputStreamWriter}

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

class FilterRequest {

  private val s3 = AmazonS3ClientBuilder.defaultClient()

  def handler(in: InputStream, out: OutputStream, context: Context): Unit = {
    // todo Ensure words are transformed to lower case before checking for profanity
    val logger = context.getLogger

    // todo Testing
    val input = Json.parse(in).asOpt[JsObject].getOrElse(JsObject.empty)
    logger.log(input.toString)

    val headers = Json.obj("Content-Type" -> "application/json")
    val output = Json.obj("statusCode" -> 200, "headers" -> headers,
      "body" -> "{\"field\": \"blah\"}")

    val response = Json.prettyPrint(output)
    logger.log(s"response: $response")

    val writer = new OutputStreamWriter(out)

    writer.write(response)
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
