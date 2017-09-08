package top

import java.io.{InputStream, OutputStream, OutputStreamWriter}

import com.amazonaws.services.s3.AmazonS3ClientBuilder
import play.api.libs.json.{JsNumber, JsObject, Json}

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

class FilterRequest {

  private val s3 = AmazonS3ClientBuilder.defaultClient()

  def handler(in: InputStream, out: OutputStream): Unit = {
    // todo Ensure words are transformed to lower case before checking

    // todo Testing - just print the input and echo it back
    val input = Json.parse(in).asOpt[JsObject].getOrElse(JsObject.empty).value
    println(input)

    val output = JsObject(Map("statusCode" -> JsNumber(200), "body" -> JsObject(input)))

    val ow = new OutputStreamWriter(out)

    ow.write(Json.prettyPrint(output))
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
