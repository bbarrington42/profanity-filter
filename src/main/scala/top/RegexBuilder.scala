package top

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

class RegexBuilder {

  private val s3 = AmazonS3ClientBuilder.defaultClient()

  def build(event: S3Event): Unit = {

    val bucketAndKey = event.getRecords.asScala.headOption.map(record => {
      val s3Entity = record.getS3
      (s3Entity.getBucket.getName, s3Entity.getObject.getKey)
    })

    bucketAndKey.foreach { case (bucket, key) =>

      println(s"bucket: $bucket, key: $key")

      val profanityList = getProfanityList(bucket, key)

      val s = profanityList.mkString(", ")
      println(s)
    }
  }

  implicit val readProfanityList = new Reads[Seq[String]] {
    override def reads(json: JsValue) = try {
      val entries = json.as[JsArray].value.map(e => e.as[String])
      JsSuccess(entries)
    } catch {
      case e: Throwable => JsError(e.getMessage)
    }
  }

  private def getProfanityList(bucket: String, key: String): Seq[String] = {
    val json = s3.getObjectAsString(bucket, key)
    println(json)
    Json.parse(json).as[Seq[String]]
  }
}
