package org.lists

/*
  P04 Find the number of elements of a list.
  Example:
  scala> length(List(1, 1, 2, 3, 5, 8))
  res0: Int = 6
 */
object P04 {

  def findLength[T](list: List[T]): Int = list match {
    case Nil => 0
    case _ :: tail => 1 + findLength(tail)
  }

  def main(args: Array[String]): Unit = {
    val list = List(1,1,2,3,5,8)
    // built-in
    println(list.length)
    // using own function
    println(findLength(list))
  }
}
