package org.s99.lists

/*
P17 (*) Split a list into two parts.
The length of the first part is given. Use a Tuple for your result.
Example:
     scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h,
 */
object P17 {

  //easy built in way
  def splitEasy[T](n: Int, list: List[T]): (List[T], List[T]) =
    list.splitAt(n)

  //scala99 way
  def splitRecursive[T](n: Int, list: List[T]): (List[T], List[T]) = (n, list) match {
    case (_, Nil) => (Nil, Nil)
    case (0, list) => (Nil, list)
    case (n, h :: tail) => {
      val (pre, post) = splitRecursive(n - 1, tail)
      (h :: pre, post)
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    println(splitEasy(3, list))
    println(splitRecursive(3, list))
  }

}
