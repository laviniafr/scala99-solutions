package org.lists

/*
P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.
Example:
     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), Li
 */
object P09 {

  // recursive
  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case h :: tail => List.from(h:: tail.takeWhile(_ ==h)) :: pack(tail.dropWhile(_ == h))
  }


  def main(args: Array[String]): Unit = {
    val list = List('a', 'a', 'a', 'a', 'b', 'c', 'c','c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    println(pack(list))

  }
}
