package org.lists

import P26._

/*
(**) Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
Write a function that generates all the possibilities.
Example:
scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
b) Generalize the above predicate in a way that we can specify a list of group sizes
and the predicate will return a list of groups.
Example:
     scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip"
     res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David),
 */

object P27 {

  // my solution for a. using P26
  def group3[String](list: List[String]): List[List[List[String]]] = {
    val g2 = combs(2, list)
    val g3 = combs(3, list)
    val g4 = combs(4, list)
    for (x <- g2; y <- g3; z <- g4 if ((x ::: y ::: z).length == (x ::: y ::: z).distinct.length))
      yield List(x, y, z)
  }

  // scala 99 solution for a.
  def group399[A](ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(2, ls)
      noA = (ls.toSet -- a).toList
      b <- combinations(3, noA)
    } yield List(a, b, (noA.toSet -- b).toList)


  // my solution for b. using my solution for a

  def group[String](sizes: List[Int],list: List[String]): List[List[List[String]]] = {
    val (a :: b :: c) = sizes
    val g2 = combs(a, list)
    val g3 = combs(b, list)
    val g4 = combs(c.head, list)
    for (x <- g2; y <- g3; z <- g4 if ((x ::: y ::: z).length == (x ::: y ::: z).distinct.length))
      yield List(x, y, z)
  }

  // scala99 solution for b
  def group99[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
    case Nil     => List(Nil)
    case n :: ns => combinations(n, ls) flatMap { c =>
      group(ns, (ls.toSet -- c).toList) map {c :: _}
    }
  }



  def main(args: Array[String]): Unit = {
    val list = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    println(group3(list).length)
//    println(group399(list).length)
    println(group(List(2,3,4),list).length)
//    println(group99(List(2,3,4),list).length)

  }

}
