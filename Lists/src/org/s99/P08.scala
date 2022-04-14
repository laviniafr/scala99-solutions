package org.s99

import scala.annotation.tailrec

/*
P08 (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
 */
object P08 {

  // my version with pattern matching
  def eliminate[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case h :: e :: tail if h == e => eliminate(h :: tail)
    case h :: tail => h :: eliminate(tail)
  }

  // using scala99 version
  def eliminate1[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case h :: tail => h :: eliminate1(tail.dropWhile(_ == h))
  }

  def main(args: Array[String]): Unit = {
    val list = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    println(eliminate(list))
  }
}
