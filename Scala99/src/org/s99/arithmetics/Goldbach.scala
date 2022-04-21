package org.s99.arithmetics

/*
  Includes P40 and P41.
 */
import Primes._
import org.s99.lists.P26.combinations

import scala.language.implicitConversions

class Goldbach(val x: Int) {

  import Goldbach._

  /*
    P40 (**) org.s99.arithmetics.Goldbach's conjecture.
    org.s99.arithmetics.Goldbach's conjecture says that every positive even number greater than 2 is the sum
    of two prime numbers. E.g. 28 = 5 + 23. It is one of the most famous facts in number
    theory that has not been proved to be correct in the general case. It has been numerically
    confirmed up to very large numbers (much larger than Scala's Int can represent).

    Write a function to find the two prime numbers that sum up to a given even integer.
    scala> 28.goldbach
    res0: (Int, Int) = (5,23)
   */
  /*
   My version, using findPrimes from org.s99.arithmetics.Primes and combinations from P26.
   Problem asks for the pair, this solution finds all possible pairs that sum up to given number.
   */
  def goldbach: List[(Int, Int)] = {
    val combs = combinations(2, x.findPrimes)
    combs.filter(_.sum == x).map(li => (li.head, li(1)))
  }

  /*
    P41 (**) A list of org.s99.arithmetics.Goldbach compositions.
    Given a range of integers by its lower and upper limit, print a list of all even numbers and their org.s99.arithmetics.Goldbach composition.
    scala> printGoldbachList(9 to 20)
    10 = 3 + 7
    12 = 5 + 7
    14 = 3 + 11
    16 = 3 + 13
    18 = 5 + 13
    20 = 3 + 17
   */

  def printGoldBachList(y: Int): Any = {
    def printNicely(a: Int, b: Int, c: Int): Unit = {
      println(a + " = " + b + " + " + c)
    }

    for (i <- x to y if i % 2 == 0) {
      val a = i.goldbach
      printNicely(i, a.head._1, a.head._2)
    }
  }

  /*
  In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.

  Example (minimum value of 50 for the primes):

  scala> printGoldbachListLimited(1 to 2000, 50)
  992 = 73 + 919
  1382 = 61 + 1321
  1856 = 67 + 1789
  1928 = 61 + 1867

   */
  /*
    Filter both primes such that they're bigger than y.
   */
  def goldbachLimited(y: Int): List[(Int, Int)] = {
    val combs = combinations(2, x.findPrimes)
    combs.filter(_.sum == x).map(li => (li.head, li(1))).filter(_._1 > y).filter(_._2 > y)
  }

  def printGoldBachListLimited(y: Int, z: Int): Any = {
    def printNicely(a: Int, b: Int, c: Int): Unit = {
      println(a + " = " + b + " + " + c)
    }

    for (i <- x to y if i % 2 == 0) {
      val a = i.goldbachLimited(z)
      if (a.nonEmpty)
        printNicely(i, a.head._1, a.head._2)
    }
  }


}

object Goldbach {
  implicit def int2Goldbach(i: Int): Goldbach = new Goldbach(i)

  val p40: List[(Int, Int)] = 28.goldbach

  def main(args: Array[String]): Unit = {
    println(p40)
    // test p41
    9.printGoldBachList(20)

    // test 41 limited
    1.printGoldBachListLimited(2000, 50)


  }
}
