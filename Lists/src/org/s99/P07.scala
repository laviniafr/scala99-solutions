package org.s99

/*
  P07 (**) Flatten a nested list structure.
  Example:
  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  res0: List[Any] = List(1, 1, 2, 3, 5, 8)
 */
object P07 {

  def flatten(list: List[Any]): List[Any] = list flatMap {
    case li: List[_] => flatten(li)
    case e => List(e)
  }

  def main(args: Array[String]): Unit = {
    val nested = List(List(1, 1), 2, List(3, List(5, 8)))
    println(flatten(nested))
  }
}
