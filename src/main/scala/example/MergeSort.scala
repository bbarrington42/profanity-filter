package example

/**
  * Created by bbarrington on 7/7/17.
  */
object MergeSort {

  def combine(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, Nil) => Nil
    case (Nil, r) => r
    case (l, Nil) => l
    case (hdl::tll, hdr::tlr) => if (hdl < hdr) hdl :: combine(tll, r) else hdr :: combine(l, tlr)
  }

  def sort(l: List[Int]): List[Int] = l match {
    case Nil => l
    case hd::Nil => l
    case _ => {
      val (a, b) = l.splitAt(l.length/2)
      combine(sort(a), sort(b))
    }
  }

  def main(args: Array[String]): Unit = {
    val l = List(8,4,2,9,5,7,1,7)
    println(sort(l))
  }
}
