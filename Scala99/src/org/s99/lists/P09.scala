package org.s99.lists

/*
P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.
Example:
     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), Li
 */
object P09 {

  // my version
  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case h :: tail => List.from(h :: tail.takeWhile(_ == h)) :: pack(tail.dropWhile(_ == h))
  }

  //scala99 version, using span
  def pack1[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) List(List())
    else {
      val (packed, next) = list span {_ == list.head}
      if(next==Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    println(pack(list))

  }
}
