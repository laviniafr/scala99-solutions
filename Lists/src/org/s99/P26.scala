package org.s99

/*
(**) Generate the combinations of K distinct objects chosen from the N elements of a list.
    In how many ways can a committee of 3 be chosen from a group of 12 people?
    We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known
    binomial coefficient). For pure mathematicians, this result may be great. But we want
    to really generate all the possibilities.
    Example:
     scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
     res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b,...
 */
object P26 {

  //my version
  //generate subsets of given size
  def combs[T](k: Int, list: List[T]): List[List[T]] =
    list.toSet[T].subsets(k).map(_.toList).toList

  // the scala99 solution
  // helper method: similar to flatMap, but passes successive sublists instead of each element
  def flatMapSublists[A, B](list: List[A])(f: (List[A]) => List[B]): List[B] =
    list match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](k: Int, list: List[A]): List[List[A]] =
    if (k == 0) List(Nil)
    else flatMapSublists(list) { sublist =>
      combinations(k - 1, sublist.tail) map {
        sublist.head :: _
      }
    }


  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd', 'e', 'f')
    println(combs(3, list))
  }
}
