package org.s99

import org.s99.P20.removeAt

import scala.util.Random

/*
(**) Extract a given number of randomly selected elements from a list. Example:
     scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
     res0: List[Symbol] = List('e, 'd, 'a)

 */
object P23 {

  // my first attempt using P20
  def randomSelect[T](n: Int, list: List[T]): List[T] =
    if (n <= 0) Nil
    else {
      val random = new Random().between(0, list.length)
      val (ls, num) = removeAt(random, list)
      num :: randomSelect(n - 1, ls)
    }

  // my second attempt but with pattern matching
  def randomSelect2[T](n: Int, list: List[T]): List[T] = (n, list) match {
    case (_, Nil) => throw new NoSuchElementException
    case (0, _) => Nil
    case (n, list) => {
      val (ls, num) = removeAt(new Random().between(0, list.length), list)
      num :: randomSelect2(n - 1, ls)
    }
  }

  // scala99 version: it can be expensive to create a new random instance each time, so only do it once
  def randomSelect3[T](n: Int, list: List[T]): List[T] = {
    def randomSelectR(n: Int, list: List[T], r: Random): List[T] =
      if (n <= 0) Nil
      else {
        val (ls, num) = removeAt(r.nextInt(list.length), list)
        num :: randomSelectR(n - 1, ls, r)
      }

    randomSelectR(n, list, new Random)
  }

  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    println(randomSelect3(3, list))
  }
}
