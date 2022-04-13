package org.lists

import scala.annotation.tailrec

/*
P16 (**) Drop every Nth element from a list. Example:
     scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 */
object P16 {

  //my version
  /*

   */
  def dropN[T](list: List[T], n: Int): List[Any] =
  if (list.isEmpty) Nil
  else {
    list.zipWithIndex.collect {
      case (e,i) if (i+1)%n !=0 => e
    }
  }
  def main(args: Array[String]): Unit = {
    val list = List('a','b','c','d','e','f','g','h','i','j','k')
//    println(list.indexOf(2))
    println(dropN(list, 3))
  }

}
