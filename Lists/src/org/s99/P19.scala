package org.s99

import scala.annotation.tailrec

/*
(**) Rotate a list N places to the left. Examples:
scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
 */
object P19 {

  //my attempt
  @tailrec
  def rotate[T](n: Int, list: List[T]): List[T] = (n, list) match {
    case (_, Nil) => Nil
    case (0, list) => list
    case (n, list) if n < 0 => rotate(n + 1, list.reverse.head :: list.reverse.tail.reverse)
    case (n, h :: tail) if n > 0 => rotate(n - 1, (h :: tail.reverse).reverse)
  }

  //scala99
  def rotate_s[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotate(nBounded + ls.length, ls)
    else (ls drop nBounded) ::: (ls take nBounded)
  }

  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    println(rotate(-4, list))
  }

}
