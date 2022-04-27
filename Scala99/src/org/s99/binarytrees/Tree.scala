package org.s99.binarytrees

sealed abstract class Tree[+T]{
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]

}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  override def addValue[U >: T <% Ordered[U]](x: U): Node[U] ={
    if (x<value) {
      if(left == End)
        Node(value, End.addValue(x), right)
      else
        Node(value, left.addValue(x),right)
    }
    else {
      if(right==End)
        Node(value, left, End.addValue(x))
      else
        Node(value, left, right.addValue(x))
    }
  }


}

case object End extends Tree[Nothing] {
  override def toString = "."

  override def addValue[U >: Nothing <% Ordered[U]](x: U): Node[U] = Node(x, End, End)
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

