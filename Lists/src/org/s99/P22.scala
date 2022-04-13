package org.s99

/*
(*) Create a list containing all integers within a given range. Example:
     scala> range(4, 9)
     res0: List[Int] = List(4, 5, 6, 7, 8, 9)
 */
object P22 {

  // most basic version
  def newRange(a: Int, b: Int): List[Int] = List.range(a, b)

  // my attempt at pattern matching
  def newRange2(a: Int, b: Int): List[Int] = (a, b) match {
    case (0, 0) => throw new NoSuchElementException
    case (a, b) if (a == b) => List(a)
    case (a, b) => a :: newRange2(a + 1, b)
  }


  def main(args: Array[String]): Unit = {
    println(newRange2(4, 9))

  }
}
