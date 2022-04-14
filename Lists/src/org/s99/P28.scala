package org.s99
import P10.encode99
import P12.decode99
/*
P28 (**) Sorting a list of lists according to length of sublists.
a) We suppose that a list contains elements that are lists themselves.
The objective is to sort the elements of the list according to their length.
E.g. short lists first, longer lists later, or vice versa.

Example:

scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
b) Again, we suppose that a list contains elements that are lists themselves.
 But this time the objective is to sort the elements according to their length
 frequency; i.e. in the default, sorting is done ascendingly, lists with rare
 lengths are placed, others with a more frequent length come later.

Example:

scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
 */
object P28 {

  // my first attempt at a, messy stuff
  def lsort[T](list: List[List[T]]):List[List[T]] = {
    def getLengths(list: List[List[T]]): List[Int] = list match {
      case Nil => List(0)
      case h ::tail => h.length :: getLengths(tail)
    }
    val pairs = list zip getLengths(list)
    pairs.sortBy(_._2).map(_._1)
  }

  //my solution for a), which is the attempt above now cleaned up since there's no need for the inner pattern matching function
  //pair the list of lists with the list of lengths, sort them by the length then map to output the sorted list of lists
  def lsort1[T](list: List[List[T]]):List[List[T]] = {
    list.zip(list.map(a => a.length)).sortBy(_._2).map(_._1)
  }

  // my solution for b), pretty ugly using P10 and P12
  def lsortFreq[T](list: List[List[T]]): Any = {
    val frequencies = decode99(encode99(list.map(a => a.length).sorted).sortBy(_._1))
//    val pairs = list.zip(list.map(a => a.length))
    frequencies.map(x => list.find(_.length==x).get)
  }

  // to do: Check out the scala99 solutions

  def main(args: Array[String]): Unit = {
    val list = List(List('a', 'b', 'c'), List('d', 'e'), List('f', 'g', 'h'), List('d', 'e'), List('i', 'j', 'k', 'l'), List('m', 'n'), List('o'))
    println(lsort1(list))
    println(lsortFreq(list))
  }
}
