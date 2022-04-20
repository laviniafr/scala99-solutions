package org.s99.arithmetics

/*
Includes P32, P33, P34
 */

import scala.annotation.tailrec
import scala.language.implicitConversions


class Coprimes(val e1: Int, val e2: Int) {

  /*
  P32 (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
     scala> gcd(36, 63)
     res0: Int = 9
   */

  /*
    My version, using subtraction
   */
  def gcd: Int = {
    @tailrec
    def gcdIn(a: Int, b: Int): Int =
      if (a == b) a
      else if (a > b) gcdIn(a - b, b)
      else gcdIn(a, b - a)

    gcdIn(e1, e2)
  }

  /*
    Scala99 version, using mod
   */
  def gcd2: Int = {
    @tailrec
    def gcd2In(a: Int, b: Int): Int = if (b == 0) a else gcd2In(b, a % b)

    gcd2In(e1, e2)
  }

  /*
  P33 (*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
       scala> 35.isCoprimeTo(64)
       res0: Boolean = true
   */

  /*
    My version, using gcd above, same as Scala99
   */
  def isCoprime: Boolean = gcd == 1


}

object Coprimes {
  implicit def int2CoprimesInt(t: (Int, Int)): Coprimes = new Coprimes(t._1, t._2)

  val p32: Int = (36, 63).gcd
  val p33: Boolean = (35, 64).isCoprime

  /*
  P34 (**) Calculate Euler's totient function phi(m).
  Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
       scala> 10.totient
       res0: Int = 4
   */

  def totient(a: Int): Int = List.from(1 until a).count(x => (x, a).isCoprime)

  val p34: Int = totient(10)

  def main(args: Array[String]): Unit = {
    println(p32)
    println(p33)
    println(p34)
  }
}
