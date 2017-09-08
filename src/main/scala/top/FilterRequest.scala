package top

import java.io.{InputStream, OutputStream}

import com.amazonaws.services.s3.AmazonS3ClientBuilder

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
