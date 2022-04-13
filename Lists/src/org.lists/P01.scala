package org.lists

import scala.annotation.tailrec

/*
  P01 Find the last element of a list.
  Example:
  scala> last(List(1, 1, 2, 3, 5, 8))
  res0: Int = 8
 */
object P01 {

  // scala99 solution
  // The functional approach is to recurse down the list until we hit the end.
  @tailrec
  def findLast[A](list: List[A]): A = list match {
    case h :: Nil => h
    case _ :: tail => findLast(tail)
    case _ => throw new NoSuchElementException
  }

  def main(args: Array[String]): Unit = {
    val list: List[Int] = List(1,1,2,3,5,8)
    // using builtin
    println(list.last)
    // using rec
    println(findLast(list))
  }
}
