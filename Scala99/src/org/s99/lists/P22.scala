package org.s99.lists

import scala.annotation.tailrec

/*
(*) Create a list containing all integers within a given range. Example:
     scala> range(4, 9)
     res0: List[Int] = List(4, 5, 6, 7, 8, 9)
 */
object P22 {

  // most basic version
  def newRange(a: Int, b: Int): List[Int] = List.range(a, b)

  // my attempt at pattern matching
  def newRange1(a: Int, b: Int): List[Int] = (a, b) match {
    case (0, 0) => throw new NoSuchElementException
    case (a, b) if (a == b) => List(a)
    case (a, b) => a :: newRange2(a + 1, b)
  }

  // scala99 tail recursive version
  def newRange2(a: Int, b: Int): List[Int] = {
    @tailrec
    def rangeR(b: Int, result: List[Int]): List[Int] = {
      if (b < a) result
      else rangeR(b - 1, b :: result)
    }
    rangeR(b, Nil)
  }

  def main(args: Array[String]): Unit = {
    println(newRange1(4, 9))
  }
}
