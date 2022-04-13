package org.s99

/*
  P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
Example:
     scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
     res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e
 */
object P12 {

  //my version
  def decode[T](list: List[(Int, T)]): List[T] = list match {
    case Nil => Nil
    case h :: tail => List.fill(h._1)(h._2) ::: decode(tail)
  }

  //scala99 version
  def decode99[T](list: List[(Int, T)]): List[T] =
    list.flatMap(e => List.fill(e._1)(e._2))

  def main(args: Array[String]): Unit = {
    val list: List[(Int, Char)] = List((4, 'a'), (1, 'b'), (3, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
    println(decode(list))
  }

}
