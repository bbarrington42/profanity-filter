package top

import java.util.Locale

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder
import com.amazonaws.services.dynamodbv2.document._
import org.slf4j.LoggerFactory
import top.ItemCollectionView.ScanCollection
import top.Regexes.ProfanityRegex

import scala.collection.SeqView
import scalaz.\/

/*
  Create a view from the Iterable[Item] obtained from the ItemCollection. This will allow us to access pages
  from DynamoDB only as they are needed. If a term fails, then the remainder of the regexes do not have to be
  accessed.
 */

abstract class ItemCollectionView[T](scanCollection: ScanCollection) extends SeqView[T, Seq[T]] {

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
    private val iter = scanCollection.iterator()

    override def hasNext = iter.hasNext

    override def next = as(iter.next)
  }
}

object ItemCollectionView {
  type ScanCollection = ItemCollection[ScanOutcome]
}

class Regexes(itemCollection: ScanCollection) extends ItemCollectionView[ProfanityRegex](itemCollection) {
  override def as(item: Item): ProfanityRegex =
  // todo Perhaps put these column names in a config?
    ProfanityRegex(term = item.getString("term"), regex = item.getString("regex"))
}

object Regexes {
  case class ProfanityRegex(term: String, regex: String)

  def apply(itemCollection: ScanCollection) = new Regexes(itemCollection)
}

/*
  Interface for handling updates & reads of the profanity regexes backed by DynamoDB.
 */

class RegexSupport(locale: Locale) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val client = AmazonDynamoDBClientBuilder.defaultClient()
  private val dynamoDB = new DynamoDB(client)

  private val regexTable = dynamoDB.getTable("ProfanityRegex")

  private val profanityFilter = ProfanityFilter(locale)

  lazy val regexes = Regexes(regexTable.scan())

  // Adding a regex directly
  def addViaRegex(regex: String): Throwable \/ PutItemOutcome = \/.fromTryCatchNonFatal {
    val item = new Item().withString("regex", regex)
    regexTable.putItem(item)
  }

  // Generate a regex from the term and add both
  def addViaTerm(term: String): Throwable \/ PutItemOutcome = \/.fromTryCatchNonFatal {
    val regex = profanityFilter.build(term) // Create the regex

    logger.info(s"Adding $regex for $term")

    // Validate
    if (regex.r.findFirstMatchIn(term).isEmpty)
      throw new RuntimeException(s"Regex $regex is not valid for $term")

    val item = new Item().withString("term", term).withString("regex", regex)
    regexTable.putItem(item)
  }

  def removeViaRegex(regex: String): Throwable \/ DeleteItemOutcome = \/.fromTryCatchNonFatal {
    logger.info(s"Removing $regex")
    regexTable.deleteItem(new KeyAttribute("regex", regex))
  }

  def removeViaTerm(term: String): Throwable \/ DeleteItemOutcome = \/.fromTryCatchNonFatal(
    profanityFilter.build(term)).flatMap(removeViaRegex)

}

object RegexSupport {
  def apply(locale: Option[Locale] = None): RegexSupport = new RegexSupport(locale.getOrElse(Locale.getDefault))
}


object Test {
  def main(args: Array[String]): Unit = {
    val support = RegexSupport()

    val r = "pussy"
    println(support.addViaTerm(r))

    val iter = support.regexes.iterator
    while (iter.hasNext) println(iter.next())


    val len = support.regexes.length
    val index = len - 1

    println(s"Regex at index $index: ${support.regexes(index)}")

    println(s"Length: $len")
  }
}
