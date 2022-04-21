package org.s99.logics

import scala.language.implicitConversions

/*
P47 (*) Truth tables for logical expressions (2).
Continue problem P46 by redefining and, or, etc as operators. (i.e. make them methods
of a new class with an implicit conversion from Boolean.) not will have to be left as
a object method.
scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
A     B     result
true  true  true
true  false true
false true  false
false false false
 */

import P46.table2
import P46.not

class P47(val a: Boolean) {

  import P47._

  def and(b: Boolean): Boolean = P46.and(a, b)

  def or(b: Boolean): Boolean = P46.or(a, b)

  def nand(b: Boolean): Boolean = P46.nand(a, b)

  def nor(b: Boolean): Boolean = P46.nor(a, b)

  def xor(b: Boolean): Boolean = P46.xor(a, b)

  def impl(b: Boolean): Boolean = P46.impl(a, b)

  def equ(b: Boolean): Boolean = P46.equ(a, b)

}

object P47 {
  implicit def booleanToP47(a: Boolean): P47 = new P47(a)

  def main(args: Array[String]): Unit = {
    table2((a: Boolean, b: Boolean) => a and (a or not(b)))
  }
}