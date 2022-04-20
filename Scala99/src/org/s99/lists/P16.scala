package org.s99.lists

/*
P16 (**) Drop every Nth element from a list. Example:
     scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 */
object P16 {

  //my version
  //pairs the elements with their index and chooses the elements for which the index+1 is a multiple of n
  def dropN[T](list: List[T], n: Int): List[Any] =
  if (list.isEmpty) Nil
  else {
    list.zipWithIndex.collect {
      case (e, i) if (i + 1) % n != 0 => e
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    println(dropN(list, 3))
  }

}
