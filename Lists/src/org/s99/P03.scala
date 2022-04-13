package org.s99

import scala.annotation.tailrec

/*
  P03(*) Find the Kth element of a list.
  By convention, the first element in the list is element 0.
  Example:

  scala> nth(2, List(1, 1, 2, 3, 5, 8))
  res0: Int = 2
 */
object P03 {

  // my version with built-ins
  def nth[T](n: Int, li: List[T]): T = li.take(n + 1).last

  // scala99 version without built-ins
  @tailrec
  def fancyNth[A](list: List[A], n: Int): A = (n, list) match {
    case (0, h :: _) => h
    case (n, _ :: tail) => fancyNth(tail, n - 1)
    case (_, Nil) => throw new NoSuchElementException
  }

  def main(args: Array[String]): Unit = {
    //    println(nth(2, List(1, 1, 2, 3, 5, 8)))
    println(fancyNth(List(1, 1, 2, 3, 5, 8), 5))
  }
}
