package top

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder
import com.amazonaws.services.dynamodbv2.document._
import top.ItemCollectionView.ScanCollection

import scala.collection.SeqView
import scalaz.\/

/*
  Create a view from the Iterable[Item] obtained from the ItemCollection. This will allow us to access pages
  from DynamoDB only as they are needed. If a term fails, then the remainder of the regexes do not have to be
  accessed.
 */

abstract class ItemCollectionView[T](itemCollection: ScanCollection) extends SeqView[T, Seq[T]] {

  // Conversion from Item elements
  def as(item: Item): T

  override def length = {
    val iter = iterator
    var i = 0
    while (iter.hasNext) {
      iter.next
      i += 1
    }
    i
  }

  override def apply(idx: Int) = {
    if (idx < 0) throw new IndexOutOfBoundsException(idx.toString)
    val iter = iterator
    var i = idx
    while (iter.hasNext && i > 0) {
      iter.next
      i -= 1
    }
    if (i == 0) iter.next else throw new IndexOutOfBoundsException(idx.toString)
  }

  // Not sure about this...
  override protected def underlying = this.asInstanceOf[Seq[T]]

  override def iterator = new Iterator[T] {
    private val iter = itemCollection.iterator()

    override def hasNext = iter.hasNext

    override def next = as(iter.next)
  }
}

object ItemCollectionView {
  type ScanCollection = ItemCollection[ScanOutcome]
}

// todo Change repr of regexes to contain regex in binary and string format & also the value that the user types in
class Regexes(itemCollection: ScanCollection) extends ItemCollectionView[String](itemCollection) {
  override def as(item: Item): String = item.getString("regex")
}

object Regexes {
  def apply(itemCollection: ScanCollection) = new Regexes(itemCollection)
}

/*
  Experimental interface for handling updates & reads of the profanity regexes. This is backed by DynamoDB.
 */

object RegexListSupport {

  val client = AmazonDynamoDBClientBuilder.standard().build()
  val dynamoDB = new DynamoDB(client)
  val regexTable = dynamoDB.getTable("Regexes")

  lazy val regexes: Seq[String] = Regexes(regexTable.scan())

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

    println(s"index: ${regexes(4)}")

    println(regexes.length)
  }
}

