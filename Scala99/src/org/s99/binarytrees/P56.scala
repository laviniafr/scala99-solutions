/*
  P56 (**) Symmetric binary trees.
  Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether a given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
  scala> Node('a', Node('b'), Node('c')).isSymmetric
  res0: Boolean = true
 */
package org.s99.binarytrees

object P56 {

  // My version, matching all edge cases and recursing whenever there is a possibility the trees are symmetric
  def isMirrorOf[T](first: Tree[T], second: Tree[T]): Boolean = (first, second) match {
    case (Node(_, End, End), Node(_, End, End)) => true
    case (Node(_, l1, End), Node(_, l2, End)) => isMirrorOf(l1, l2)
    case (Node(_, End, r1), Node(_, End, r2)) => isMirrorOf(r1, r2)
    case (Node(_, l1, r1), Node(_, l2, r2)) => {
      isMirrorOf(l1, l2) && isMirrorOf(r1, r2)
    }
    case _ => false
  }

  def isSymmetric[T](tree: Tree[T]): Boolean = tree match {
    case End => false
    case Node(_, left, right) => isMirrorOf(left, right)
  }

  def main(args: Array[String]): Unit = {
    val tree1 = Node('a', Node('b'), Node('c'))
    val tree2 = Node('a', End, Node('c', Node('a'), End))
    val tree3 = Node("x", Node("x", Node("x", End, End), Node("x", End, End)), Node("x", Node("x", End, End), Node("x", End, End)))

//    println(isMirrorOf(Node('a', End, Node('c', Node('a'), End)), Node('a', End, Node('c', Node('a'), End))))

    println(isSymmetric(tree1))
    println(isSymmetric(tree2))
    println(isSymmetric(tree3))
  }

}
