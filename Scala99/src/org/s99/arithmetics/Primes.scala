package org.s99.arithmetics

/*
  Includes P31, P35, P36, P37, P38, P39
 */

import Coprimes.totient
import Timer.timeFunction

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.math.pow

class Primes(val x: Int) {

  import Primes._

  /*
     P31 (**) Determine whether a given integer number is prime.
     scala> 7.isPrime
     res0: Boolean = true
   */

  /*
  Finding primes using the sieve of Eratosthenes
   */
  def findPrimes: List[Int] = {
    if (x <= 1) throw new IndexOutOfBoundsException
    else {
      val boo: Array[Boolean] = Array.fill(x)(true)
      for (i <- 2 to Math.sqrt(x).toInt) {
        if (boo(i))
          for (k <- 1 until Math.sqrt(x).toInt - 1; j <- i * i to x by k * i)
            boo(j) = false
      }
      boo.zipWithIndex.collect { case (e, i) if e => i }.drop(2).toList
    }
  }

  /*
  Calculate all primes to x and compare the last prime with x.
   */
  def isPrime: Boolean =
    if (x.findPrimes.reverse.take(1).head == x) true
    else false

  /*
     P35 (**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
     scala> 315.primeFactors
     res0: List[Int] = List(3, 3, 5, 7)
   */

  def primeFactors: List[Int] = (x / 2).findPrimes.filter(i => x % i == 0)

  /*
  P36 (**) Determine the prime factors of a given positive integer (2). Construct a list containing the prime factors and their multiplicity.
     scala> 315.primeFactorMultiplicity
     res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
    Alternately, use a Map for the result.
     scala> 315.primeFactorMultiplicity
     res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
   */

  /*
   Not the prettiest but it does the job
   */
  def primeFactorsMultiplicity: List[(Int, Int)] = {
    val factors = x.primeFactors
    var xCopy = x
    val li = ArrayBuffer[Int]()
    for (i <- factors) {
      var c = 0
      while (xCopy % i == 0) {
        c += 1
        xCopy /= i
      }
      li.addOne(c)
    }
    factors.zip(li.toList)
  }

  /*
    P37 (**) Calculate Euler's totient function phi(m) (improved).
    See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m>) can be efficiently calculated as follows: Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their
    multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
    phi(m) = (p1-1)*p1^(m1-1) * (p2-1)*p2^(m2-1) * (p3-1)*p3^(m3-1) * ...
    Note that a^b stands for the bth power of a.
   */

  def phi: Int = {
    val pairs = x.primeFactorsMultiplicity

    def phiIn(pairs: List[(Int, Int)]): Int = pairs match {
      case (p, m) :: Nil => (p - 1) * pow(p, m - 1).toInt
      case (p, m) :: tail => (p - 1) * pow(p, m - 1).toInt * phiIn(tail)
    }

    phiIn(pairs)
  }

  /*
    P38 (*) Compare the two methods of calculating Euler's totient function.
    Use the solutions of problems P34 and P37 to compare the algorithms. Try to calculate phi(10090) as an example.
   */
  def comparePhi(): Unit = {
    println("x = " + x)
    println("P34 estimated time: " + timeFunction(totient(x)))
    println("P37 estimated time: " + timeFunction(x.phi))
  }
  /*
    Upon comparison, the algorithm at P37 is significantly faster as x increases.
   */

  /*
   P39 (*) A list of prime numbers.
   Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
   scala> listPrimesInRange(7 to 31)
   res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
   */

  /*
  Function to find primes between x and y, using the sieve of Eratosthenes
   */
  def findPrimesInRange(y: Int): List[Int] = y.findPrimes.filter(_ >= x)

}

object Primes {
  implicit def int2PrimesInt(i: Int): Primes = new Primes(i)

  val p31: Boolean = 67.isPrime
  val p35: List[Int] = 315.primeFactors
  val p36: List[(Int, Int)] = 315.primeFactorsMultiplicity
  val p37: Int = 10.phi
  val p39: List[Int] = 7.findPrimesInRange(31)

  def main(args: Array[String]): Unit = {
    println(p31)
    println(p35)
    println(p36)
    println(p37)
    // test P38
    //    10.comparePhi()
    //    130.comparePhi()
    //    10090.comparePhi()
    println(p39)


  }
}