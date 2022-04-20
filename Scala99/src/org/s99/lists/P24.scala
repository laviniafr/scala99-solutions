package org.s99.lists

import P23.randomSelect3

import scala.util.Random

/*
(*) Lotto: Draw N different random numbers from the set 1..M.
Example:
     scala> lotto(6, 49)
     res0: List[Int] = List(23, 1, 17, 33, 21, 37)
 */
object P24 {

  // my first version: however the numbers may repeat
  // using the scala99 p23 tip to only use one Random instance
  def lotto(n: Int, m: Int): List[Int] = {
    def lottoR(n: Int, m: Int, r: Random): List[Int] = {
      if (n <= 0) Nil
      else {
        r.nextInt(m) :: lottoR(n - 1, m, r)
      }
    }
    lottoR(n, m, new Random())
  }

  // my second version, using P23 so the numbers won't repeat
  // scala99 version is similar to this
  def lotto2(n: Int, m: Int): List[Int] = {
    if (m <= 0) throw new IndexOutOfBoundsException
    else {
      randomSelect3(n, List.range(1, m + 1))
    }
  }


  def main(args: Array[String]): Unit = {
    println(lotto2(6, 49))
  }
}
