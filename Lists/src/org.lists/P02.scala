package org.lists

/*
P02 Find the last but one element of a list.
  Example:
  scala> penultimate(List(1, 1, 2, 3, 5, 8))
  res0: Int = 5
 */
object P02 {

  def findLastButOne[A](list: List[A]): A = list match {
    case pen :: _ :: Nil => pen
    case _ :: tail => findLastButOne(tail)
    case _ => throw new NoSuchElementException
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 1, 2, 3, 5, 8)
    //using builtin
    println(list.take(list.length - 1).last)
    println(findLastButOne(list))
  }

}
