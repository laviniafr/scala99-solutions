package org.lists

import scala.annotation.tailrec

/*
  P03 Find the Kth element of a list.
  By convention, the first element in the list is element 0.
  Example:

  scala> nth(2, List(1, 1, 2, 3, 5, 8))
  res0: Int = 2
 */
object P03 {

  def nth(n: Int, li: List[Any]): Any ={
    li.take(n+1).last
  }

  @tailrec
  def fancyNth[A](list: List[A], n: Int): A = (n,list) match {
    case (0, h :: _ ) => h
    case (n, _ :: tail) => fancyNth(tail, n-1)
    case (_, Nil) => throw new NoSuchElementException
  }
  def main(args: Array[String]): Unit = {
//    println(nth(2, List(1, 1, 2, 3, 5, 8)))
   println( fancyNth(List(1, 1, 2, 3, 5, 8),5 ))
  }
}
