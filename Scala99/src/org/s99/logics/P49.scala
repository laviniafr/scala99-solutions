package org.s99.logics

/*
  P49 (**) Gray code.
  An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
  n = 1: C(1) = ("0", "1").
  n = 2: C(2) = ("00", "01", "11", "10").
  n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").
  Find out the construction rules and write a function to generate Gray codes.

     scala> gray(3)
     res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
 */
object P49 {

  def gray(n: Int): List[String] = n match {
    case 1 => List("0", "1")
    case n => gray(n - 1).map(s => "0" + s) ::: gray(n - 1).map(s => "1" + s).reverse
  }

  def main(args: Array[String]): Unit = {
    println(gray(3))
  }


}
