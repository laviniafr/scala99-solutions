package org.s99.lists

/*
P18 (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
Example:
     scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     res0: List[Symbol] = List('d, 'e, 'f, 'g)
 */
object P18 {
  //built-in solution
  def slice[T](i: Int, k: Int, list: List[T]): List[T] =
    list.slice(i, k)

  //scala99
  def sliceRecursive[T](i: Int, k: Int, list: List[T]): List[T] = (i, k, list) match {
    case (_, _, Nil) => Nil
    case (_, e, _) if e <= 0 => Nil
    case (s, e, h :: tail) if i <= 0 => h :: sliceRecursive(0, e - 1, tail)
    case (s, e, h :: tail) => sliceRecursive(s - 1, e - 1, tail)

  }


  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    //    println(slice(3,7,list))
    println(sliceRecursive(3, 7, list))
  }
}
