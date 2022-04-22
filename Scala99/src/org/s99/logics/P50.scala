package org.s99.logics

import scala.annotation.tailrec
import scala.collection.mutable

/*
    My version, a little messy but it works
*/
object P50 {


  /*
    Function to sort list of tuples by second element
   */
  def sortByLength(li: List[(String, Int)]): List[(String, Int)] = li.sortBy(_._2)


  def huff(li: List[(String, Int)]): List[(String, String)] = {
    /*
      Hashmap to contain the huffman codes for each list element
     */
    val codes = mutable.HashMap.from(li.map(_._1).zip(List.fill(li.length)("")))

    /*
      This function is meant to recurse through the list elements, adding the two smallest ones
      to create a new node and thus a new shortened list. Every node is assigned a binary value,
      each stored in the codes hashmap. Every node resulting from an addition is basically a string
      containing all the characters accumulated from each added branch, with binary codes to be
      unpacked later.
     */
    @tailrec
    def man(li: List[(String, Int)]): Unit = {
      li match {
        case h :: j :: Nil => {
          codes(h._1) = "0";
          codes(j._1) = "1";
        }
        case a :: b :: tail => {
          codes(a._1) = "0"
          codes(b._1) = "1"
          val node = (a._1 + b._1, a._2 + b._2)
          codes.addOne(node._1, "")
          man(sortByLength(node :: tail))
        }
      }
    }
    /*
      Calling the function on the sorted list.
     */
    man(sortByLength(li))

    /*
      Unpacking the chunky nodes, firstly sorted by length.
      For loop iterates through every chunky node, and subsequently through the node's characters,
      appended the node binary code of each character in the main hashmap.

     */
    val codesList = codes.toList.sortBy(_._1.length)
    for (i <- li.length until codesList.length) {
      for (chr <- codesList(i)._1) {
        codes(chr.toString) += codesList(i)._2
      }
    }

    /*
      Convert the hashmap to a list, selecting only the single character nodes along with their
      codes reversed correspondingly.
     */
    codes.toList.filter(_._1.length == 1).take(li.length).map(a => (a._1, a._2.reverse))

  }

  def main(args: Array[String]): Unit = {
    println(huff(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))))
  }

}
