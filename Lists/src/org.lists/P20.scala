package org.lists

import scala.annotation.tailrec

/*
(*) Remove the Kth element from a list.
Return the list and the removed element in a Tuple. Elements are numbered from 0.
Example:
     scala> removeAt(1, List('a, 'b, 'c, 'd))
     res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
 */
object P20 {

  // my version 1
  def removeAt[T](k: Int, list: List[T]): (List[T], T) =
    (list.take(k) ::: list.drop(k + 1), list.drop(k).head)


  // scala 99
  // this splits the list at position k, then extracts the element e and combines the two split parts without e
  def removeAt2[T](k: Int, list: List[T]): (List[T], T) = list.splitAt(k) match {
    case (Nil, _) if k < 0 => throw new NoSuchElementException
    case (pre, e :: post) => (pre ::: post, e)
    case (pre, Nil) => throw new NoSuchElementException
  }


  //scala 99 with even fewer built-ins
  // my attempted pattern version was close but i didn't think of using a variable in the last case
  def removeRec[T](k: Int, list: List[T]): (List[T], T) =
    if (k < 0) throw new NoSuchElementException
    else (k, list) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (t, e) = removeRec(k-1, list.tail)
        (list.head :: t, e)
      }
    }

  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    println(removeRec(2, list))
  }
}
