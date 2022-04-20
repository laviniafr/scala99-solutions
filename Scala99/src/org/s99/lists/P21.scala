package org.s99.lists

/*
(*) Insert an element at a given position into a list. Example:
     scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
     res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
 */
object P21 {

  // my first attempt
  def insertAt[T](e: T, k: Int, list: List[T]): List[T] =
    if (k < 0) throw new NoSuchElementException
    else {
      val (pre, post) = list.splitAt(k)
      pre ::: (e :: post)
    }

  // second attempt similar to scala99 p20
  // scala99 version of p21 is similar to this
  def insertAt2[T](e: T, k: Int, list: List[T]): List[T] = list.splitAt(k) match {
    case (Nil, _) if k < 0 => throw new NoSuchElementException
    case (pre, post) => pre ::: (e :: post)
  }


  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    println(insertAt2("new", 2, list))
  }
}
