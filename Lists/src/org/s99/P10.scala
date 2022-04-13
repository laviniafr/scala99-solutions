package org.s99

import org.s99.P09.pack

/*
P10 (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
Example:
     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 */
object P10 {

  // assume packed list according to P09
  // my version
  def encode[T](list: List[List[T]]): List[(Int, T)] = list match {
    case Nil => Nil
    case h :: tail => (h.length, h.head) :: encode(tail)
  }

  //version from scala99
  def encode99[T](list: List[T]): List[(Int, T)] =
    pack(list) map { e => (e.length, e.head) }


  def main(args: Array[String]): Unit = {
    val list = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    println(encode(pack(list)))

  }
}
