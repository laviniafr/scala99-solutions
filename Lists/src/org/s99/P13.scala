package org.s99

/*

P13 (**) Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
Example:
     scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 */
object P13 {

  //my attempt
  def encode[T](list: List[T]): List[(Int, T)] = list match {
    case Nil => Nil
    case h :: tail => ((h :: tail.takeWhile(h == _)).length, h) :: encode(tail.dropWhile(h == _))
  }

  //scala99 version

  def encodeDirect[T](list: List[T]): List[(Int, T)] =
    if (list.isEmpty) Nil
    else {
      val (packed, next) = list.span {
        _ == list.head
      }
      (packed.length, packed.head) :: encodeDirect(next)
    }

  def main(args: Array[String]): Unit = {
    val list = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    println(encode(list))

    println(encodeDirect(list))

  }
}
