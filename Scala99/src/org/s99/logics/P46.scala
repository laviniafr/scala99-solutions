package org.s99.logics

/*
  P46 (**) Truth tables for logical expressions.
  Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence)
  which return true or false according to the result of their respective operations;
  e.g. and(A, B) is true if and only if both A and B are true.

  scala> and(true, true)
  res0: Boolean = true
  scala> xor(true. true)
  res1: Boolean = false
 */
object P46 {

  /*
    Using built-in operators:
   */
  //  def and(a: Boolean, b: Boolean): Boolean = a & b
  //  def or(a: Boolean, b: Boolean): Boolean = a | b
  //  def nand(a: Boolean, b: Boolean): Boolean = !(a & b)
  //  def nor(a: Boolean, b: Boolean): Boolean = !(a|b)
  //  def xor(a: Boolean, b: Boolean): Boolean = or(a,b) & nand(a,b)
  //  def impl(a: Boolean, b: Boolean): Boolean = !a | b
  //  def equ(a: Boolean, b: Boolean): Boolean = impl(a,b) & impl(b,a)

  /*
    Using pattern matching
   */
  def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def nand(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, true) => false
    case _ => true
  }

  def nor(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (false, false) => true
    case _ => false
  }

  def xor(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (a, b) if a == b => false
    case _ => true
  }

  def impl(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, false) => false
    case _ => true
  }

  def equ(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (a, b) if a == b => true
    case _ => false
  }

  /*
    Now, write a function called table2 which prints the truth table of a given logical expression in two variables.

    scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    A     B     result
    true  true  true
    true  false true
    false true  false
    false false false
   */
  def table2(f: (Boolean, Boolean) => Boolean): Unit = {
    println("A\t\tB\t\tResult")
    println(true + "\t" + true + "\t" + f(true, true))
    println(true + "\t" + false + "\t" + f(true, false))
    println(false + "\t" + true + "\t" + f(false, true))
    println(false + "\t" + false + "\t" + f(false, false))
  }

  def main(args: Array[String]): Unit = {
    //    println(equ(a = false, b = false))
    //    println(equ(a = false, b = true))
    //    println(equ(a = true, b = false))
    //    println(equ(a = true, b = true))
    table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
  }

}
