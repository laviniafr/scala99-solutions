package org.s99

import scala.annotation.tailrec

/*
  P05 Find out whether a list is a palindrome.
  Example:
  scala> isPalindrome(List(1, 2, 3, 2, 1))
  res0: Boolean = true

 */
object P05 {

  def isPalindrome(list: List[Int]): Boolean = {
    if (list.length % 2 != 0)
      list.take(list.length / 2).reverse equals list.drop(list.length / 2 + 1)
    else
      list.take(list.length / 2).reverse equals list.drop(list.length / 2)
  }

  @tailrec
  def isPal[T](list: List[T]): Boolean = list match {
    case h :: Nil => true
    case h :: t if h == t.last => isPal(list.slice(1, list.length - 1))
    case _ => false
  }

  def main(args: Array[String]): Unit = {
    var list = List(1, 2, 3, 2, 1)
    //    println(isPalindrome(list))
    println(isPal(list))
  }
}
