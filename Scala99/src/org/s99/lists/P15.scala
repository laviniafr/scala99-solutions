package org.s99.lists

/*
P15(**) Duplicate the elements of a list a given number of times.
Example:
scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
 */
object P15 {

  //my version, generalising my p14 solution
  def nplicate[T](list: List[T], n: Int): List[T] =
    if (list.isEmpty) Nil
    else list.flatMap(e => List.fill(n)(e))

  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd')
    println(nplicate(list, 6))
  }
}
