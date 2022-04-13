package org.lists

import P09.pack
import P10.encode99
/*
P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
Example:
scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, '
     res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 */
object P11 {

  val list = List('a', 'a', 'a', 'a', 'b', 'c', 'c','c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')

  //my version
  def encode[T](list: List[List[T]]): List[Any] = list match {
    case Nil => Nil
    case h::tail if h.length ==1 => h.head :: encode(tail)
    case h::tail if h.length >1 => (h.length, h.head) :: encode(tail)

  }
  //my second version inspired by scala99 p10
  def encode90[T](list: List[T]): List[Any] = {
    pack(list) map(e => if(e.length>1) (e.length, e.head) else e.head)
  }

  //a fun version from scala99 - more typesafe

  def encodeFun[T](list: List[T]): List[Either[T, (Int, T)]] =
    encode99(list) map { t => if (t._1 == 1) Left(t._2) else Right(t)}



  def main(args: Array[String]): Unit = {
    val list = List('a', 'a', 'a', 'a', 'b', 'c', 'c','c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    println(encode90(list))
  println(encodeFun(list))
  }

}
