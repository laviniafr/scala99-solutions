package org.s99.lists

/*
  P05 (*) Reverse a list.
  Example:
  scala> reverse(List(1, 1, 2, 3, 5, 8))
  res0: List[Int] = List(8, 5, 3, 2, 1, 1)
 */
object P06 {

  // my version
  def rev[T](list: List[T]): List[T] = list match {
    case h :: Nil => List(h)
    case h :: t => t.last :: rev(list.take(list.length - 1))
  }

  // o(n2) version from scala99
  def revRecursive[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: tail => revRecursive(tail) ::: List(head)
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 1, 2, 3, 5, 8)
    //    println(list.reverse)
    println(rev(list))
    println(revRecursive(list))
  }
}
