/*
(**) Binary search trees (dictionaries).
Write a function to add an element to a binary search tree.
scala> End.addValue(2)
res0: Node[Int] = T(2 . .)

scala> res0.addValue(3)
res1: Node[Int] = T(2 . T(3 . .))

scala> res1.addValue(0)
res2: Node[Int] = T(2 T(0 . .) T(3 . .))
Hint: The abstract definition of addValue in Tree should be def addValue[U >: T <% Ordered[U]](x: U): Tree[U]. The >: T is because addValue's parameters need to be contravariant in T. (Conceptually, we're adding nodes above existing nodes. In order for the subnodes to be of type T or any subtype, the upper nodes must be of type T or any supertype.) The <% Ordered[U] allows us to use the < operator on the values in the tree.

Use that function to construct a binary tree from a list of integers.

scala> Tree.fromList(List(3, 2, 5, 7, 1))
res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
 */
package org.s99.binarytrees

object P57 {

  // Solution overall works, but since view bounds are deprecated, I need to figure out how to use implicit conversions recursively
  // ...or change the solution altogether

  def fromList[U >: Nothing <% Ordered[U]](list: List[U]): Tree[U] =
    list.tail.foldLeft(Node.apply(list.head))((a, b) => a.addValue(b))

  def main(args: Array[String]): Unit = {
    println(End.addValue(3).addValue(2).addValue(0))
    println(fromList(List(3, 2, 5, 7, 1)))
  }


}
