package example


object Lists {

  /**
    * This method computes the sum of all elements in the list xs. There are
    * multiple techniques that can be used for implementing this method, and
    * you will learn during the class.
    *
    * For this example assignment you can use the following methods in class
    * `List`:
    *
    * - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
    * - `xs.head: Int` returns the head element of the list `xs`. If the list
    * is empty an exception is thrown
    * - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
    * list `xs` without its `head` element
    *
    * ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
    * solution.
    *
    * @param xs A list of natural numbers
    * @return The sum of all elements in `xs`
    */
  def sum(xs: List[Int]): Int = if (xs.isEmpty) 0 else xs.head + sum(xs.tail)

  /**
    * This method returns the largest element in a list of integers. If the
    * list `xs` is empty it throws a `java.util.NoSuchElementException`.
    *
    * You can use the same methods of the class `List` as mentioned above.
    *
    * ''Hint:'' Again, think of a recursive solution instead of using looping
    * constructs. You might need to define an auxiliary method.
    *
    * @param xs A list of natural numbers
    * @return The largest element in `xs`
    * @throws java.util.NoSuchElementException if `xs` is an empty list
    */
  def max(xs: List[Int]): Int = {
    def max(m: Int, l: List[Int]): Int =
      if(l.isEmpty) m else max(Math.max(m, l.head), l.tail)

    if (xs.isEmpty) throw new NoSuchElementException
    max(xs.head, xs.tail)
  }

  def avg(xs: List[Int]): Int = {
    def _avg(xs: List[Int], acc: (Int, Int)): (Int, Int) = xs match {
      case Nil => acc
      case h :: t => _avg(t, (acc._1 + 1, acc._2 + h))
    }
    val r = _avg(xs, (0, 0))
    r._2 / r._1
  }

  def main(args: Array[String]): Unit = {
    val l = List(1,2,3,4,5,6,7,8,9)
    val r = avg(l) == sum(l) / l.length

    println(r)
  }
}
