package top

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder
import com.amazonaws.services.dynamodbv2.document.DynamoDB

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

/*
  Experimental interface for handling updates & reads of the profanity regexes. This is backed by DynamoDB.
 */

object RegexListSupport {

  val client = AmazonDynamoDBClientBuilder.standard().build()
  val dynamoDB = new DynamoDB(client)
  val regexTable = dynamoDB.getTable("Regexes")

  // todo As a proof of concept, just read in ALL of the regexes
  lazy val regexes: Seq[String] = {
    val iter = iterator()
    val lb = ListBuffer.empty[String]
    while (iter.hasNext) lb += iter.next()
    lb
  }

  // todo
  // Should validate each regex
  def update() = {}

  private def iterator(): Iterator[String] = {
    val collection = regexTable.scan()
    collection.iterator().asScala.map(_.getString("regex"))
  }

}

