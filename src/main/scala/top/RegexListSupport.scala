package top

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder
import com.amazonaws.services.dynamodbv2.document._

import scala.collection.SeqView
import scalaz.\/

abstract class ItemCollectionView[T, R](itemCollection: ItemCollection[R]) extends SeqView[T, Seq[T]] {

  // Conversion from Item elements
  def as(item: Item): T

  override def length = {
    val iter = iterator
    var i = 0
    while (iter.hasNext) i += 1
    i
  }

  override def apply(idx: Int) = {
    if (idx < 0) throw new IndexOutOfBoundsException(idx.toString)
    val iter = iterator
    var i = idx
    while (iter.hasNext && i > 0) i -= 1
    if (i == 0) iter.next else throw new IndexOutOfBoundsException(idx.toString)
  }

  override protected def underlying = this.asInstanceOf[Seq[T]]

  override def iterator = new Iterator[T] {
    private val iter = itemCollection.iterator()

    override def hasNext = iter.hasNext

    override def next = as(iter.next)
  }
}

class Regexes(itemCollection: ItemCollection[ScanOutcome]) extends ItemCollectionView[String, ScanOutcome](itemCollection) {
  override def as(item: Item): String = item.getString("regex")
}

/*
  Experimental interface for handling updates & reads of the profanity regexes. This is backed by DynamoDB.
 */

object RegexListSupport {

  val client = AmazonDynamoDBClientBuilder.standard().build()
  val dynamoDB = new DynamoDB(client)
  val regexTable = dynamoDB.getTable("Regexes")

  // todo As a proof of concept, just read in ALL of the regexes
  lazy val regexes: Seq[String] = new Regexes(regexTable.scan())

  // todo
  // Should validate each regex & store in binary format
  def add(regex: String): Throwable \/ PutItemOutcome = \/.fromTryCatchNonFatal {
    regex.r // Validate
    val item = new Item().withString("regex", regex)
    regexTable.putItem(item)
  }

  def main(args: Array[String]): Unit = {
    val r = """pu(\$|s)(\$|s)."""
    add(r)

    val iter = regexes.iterator
    while (iter.hasNext) {
      println(iter.next())
    }
  }
}

