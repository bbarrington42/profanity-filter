package top

import java.util.Locale

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
    var len = 0
    while (iter.hasNext) {
      iter.next
      len += 1
    }
    len
  }

  override def apply(idx: Int) = {
    if (idx < 0) throw new IndexOutOfBoundsException(idx.toString)
    val iter = iterator
    var i = idx
    while (iter.hasNext && i > 0) {
      iter.next
      i -= 1
    }
    if (iter.hasNext) iter.next else throw new IndexOutOfBoundsException(idx.toString)
  }

  // This implementation doesn't really have an underlying type. Just point it to itself.
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

case class ProfanityRegex(term: String, regex: String)

class Regexes(itemCollection: ScanCollection) extends ItemCollectionView[ProfanityRegex](itemCollection) {
  override def as(item: Item): ProfanityRegex =
    // todo Perhaps put these column names in a config?
    ProfanityRegex(term = item.getString("term"), regex = item.getString("regex"))
}

object Regexes {
  def apply(itemCollection: ScanCollection) = new Regexes(itemCollection)
}

/*
  Interface for handling updates & reads of the profanity regexes backed by DynamoDB.
 */

class RegexListSupport(locale: Locale) {

  val client = AmazonDynamoDBClientBuilder.defaultClient()
  val dynamoDB = new DynamoDB(client)

  // todo Provide table name via the environment
  val regexTable = dynamoDB.getTable("ProfanityRegex")

  val profanityFilter = ProfanityFilter(locale)

  lazy val regexes = Regexes(regexTable.scan())

  def add(term: String): Throwable \/ PutItemOutcome = \/.fromTryCatchNonFatal {
    val regex = profanityFilter.build(term) // Create the regex
    // todo Add validation here...

    val item = new Item().withString("term", term).withString("regex", regex)
    regexTable.putItem(item)
  }

  def removeViaRegex(regex: String): Throwable \/ DeleteItemOutcome = \/.fromTryCatchNonFatal (
    regexTable.deleteItem(new KeyAttribute("regex", regex))
  )

  def removeViaTerm(term: String): Throwable \/ DeleteItemOutcome = \/.fromTryCatchNonFatal (
    profanityFilter.build(term)).flatMap(removeViaRegex)

}

object RegexListSupport {

  def apply(locale: Locale = Locale.getDefault): RegexListSupport = new RegexListSupport(locale)

  def main(args: Array[String]): Unit = {
    val support = RegexListSupport()

    val r = "pussy"
    println(support.add(r))

    val iter = support.regexes.iterator
    while (iter.hasNext) println(iter.next())


    val len = support.regexes.length

    println(s"index: ${support.regexes(len - 1)}")

    println(len)
  }
}
