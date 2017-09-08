package top

import java.util.Locale

import com.amazonaws.services.lambda.runtime.events.S3Event
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import play.api.libs.json._

import scala.collection.JavaConverters._

/*
  This lambda will detect updates to the list of profane words maintained in S3. When there is an update,
  it will retrieve the list of profane names and use these to re-generate the profanity filter regex.
  This new regex is then uploaded (in string form) into a separate bucket.
 */

/*
  Development steps:
    1 - Detect an update to the object containing the list of profane words.
    2 - Retrieve the list of profane words from S3.
    3 - Re-generate the regex.
    4 - Upload the regex to S3.
 */

class ProfanityChange {

  private val s3 = AmazonS3ClientBuilder.defaultClient()

  // todo Should be in configuration - MAKE this an environment variable!
  val KEY = "profanity.json"

  def handler(event: S3Event): Unit = {

    val records = event.getRecords.asScala
    val len = records.length
    println(s"Received $len events")

    // Filter for the one we are looking for
    val optRecord = records.filter(_.getS3.getObject.getKey == KEY).headOption
    optRecord.fold {
      println(s"$KEY not found!")
    }(r => {
      // Get the bucket name
      val bucket = r.getS3.getBucket.getName

      // Get the list of profane names
      val profanityList = getProfanityList(bucket, KEY)

      val s = profanityList.mkString(", ")
      println(s)

      // todo Question: Should we create a regex for each locale?
      val regex = ProfanityFilter(Locale.getDefault).build(profanityList)

      println(regex)

      // Store this regex in the "ccfs-profanity-regex" bucket
      // TODO This can go into the same bucket as the profanity list
      s3.putObject("ccfs-profanity-regex", "regex.txt", regex)

    })

  }

  private implicit val readProfanityList = new Reads[Seq[String]] {
    override def reads(json: JsValue) = try {
      val entries = json.as[JsArray].value.map(_.as[String])
      JsSuccess(entries)
    } catch {
      case e: Throwable => JsError(e.getMessage)
    }
  }

  private def getProfanityList(bucket: String, key: String): Seq[String] = {
    val json = s3.getObjectAsString(bucket, key)
    Json.parse(json).as[Seq[String]]
  }
}
