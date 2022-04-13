package org.lists

import P23._

import scala.reflect.ClassTag
import scala.util.Random

/*
(*) Generate a random permutation of the elements of a list.
    Hint: Use the solution of problem P23.
    Example:
     scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
     res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
 */
object P25 {

  // my attempt, using P23 - same as scala99, but with extra empty list edge case
  def randomPermute[T](list: List[T]): List[T] =
    if (list.isEmpty) Nil
    else {
      randomSelect3(list.length, list)
    }


  // the scala99 solution
  // using Fisher-Yates shuffle (efficient way of  Durstenfeld) , requires a mutable array
  def randomPermute2[T: ClassTag](list: List[T]): List[T] = {
    val random = new Random()
    val array = list.toArray
    for (i <- array.length - 1 to 1 by -1) { // from n-1 to 1
      val j = random.nextInt(i + 1) // j will store random index in the current range of i elements
      val t = array(i)  // t stores the tail value, i.e. the last element in the current list of i elements
      array.update(i, array(j)) // tail receives the element at random index
      array.update(j, t) // random index receives the tail
    }
    array.toList
  }


  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd', 'e', 'f')
    println(randomPermute(list))
    println(randomPermute2(list))
  }


}
