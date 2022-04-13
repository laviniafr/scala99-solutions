package org.lists

/*

 */
object P15 {

  //my version
  def nplicate[T](list: List[T], n: Int): List[T] =
    if(list.isEmpty) Nil
    else list.flatMap(e => List.fill(n)(e) )


  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c', 'd')
    println(nplicate(list, 6))
  }
}
