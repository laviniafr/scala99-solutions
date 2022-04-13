package org.s99

/*
P14 (*) Duplicate the elements of a list. Example:
     scala> duplicate(List('a, 'b, 'c, 'c, 'd))
     res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
 */
object P14 {

  //my version with matching
  def duplicate1[T](list: List[T]): List[T] = list match {
    case h :: tail => h :: h :: duplicate1(tail)
    case Nil => Nil
  }

  //my version using flatmap
  def duplicate2[T](list: List[T]): List[T] =
    if (list.isEmpty) Nil
    else list.flatMap(e => List.fill(4)(e))

  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd')
    println(duplicate1(list))
    println(duplicate2(list))
  }
}
