package org.s99.lists

/*
  P07 (**) Flatten a nested list structure.
  Example:
  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  res0: List[Any] = List(1, 1, 2, 3, 5, 8)
 */
object P07 {

  //my attempt with pattern matching
  def flatten[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case (h: List[T]) :: tail => flatten(h) ::: flatten(tail) //if head is also a list, flatten the head and the tail
    case h :: tail => h :: flatten(tail) // if h is not a list just flatten the tail
  }

  // scala99 version with flatmap
  def flatten1(list: List[Any]): List[Any] = list.flatMap {
    case li: List[_] => flatten1(li)
    case e => List(e)
  }

  def main(args: Array[String]): Unit = {
    val nested = List(List(1, 1), 2, List(3, List(5, 8)))
    println(flatten(nested))
  }
}
