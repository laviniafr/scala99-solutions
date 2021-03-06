/*

P55 (**) Construct completely balanced binary trees.
In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
Define an object named Tree. Write a function Tree.cBalanced to construct completely balanced binary trees for a given number of nodes. The function should generate all solutions. The function should take as parameters the number of nodes and a single value to put in all of them.

scala> Tree.cBalanced(4, "x")
res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...

 */
package org.s99.binarytrees

import org.s99.arithmetics.Timer

object P55 {

  /*
    Function covering all possible left and right pairings.
   */
  def balance(s: String, left: List[Node[String]], right: List[Node[String]]): List[Node[String]] = (s, left, right) match {
    case (s, h :: Nil, j :: Nil) => List(Node(s, h, j), Node(s, j, h))
    case (s, h :: Nil, j :: jail) => Node(s, h, j) :: Node(s, j, h) :: balance(s, List(h), jail)
    case (s, h :: tail, j :: Nil) => Node(s, h, j) :: Node(s, j, h) :: balance(s, tail, List(j))
    case (s, h :: tail, j :: jail) => Node(s, h, j) :: Node(s, j, h) :: balance(s, tail, jail) ::: balance(s, List(h), jail) ::: balance(s, List(j), tail)
  }

  /*
    My version, following the rule that f(x) = f(x/2) and f(x/2-1) branches when x is even,
    and f(x) = 2 * f(x/2) when x is odd.
    Overall not the most efficient, as some pairings are counted twice, and to avoid this I've used .distinct which is kind of cheating :)
   */
  def cBalanced[T](n: Int, s: String): List[Node[String]] = n match {
    case 1 => List(Node.apply(s))
    case 2 => List(Node(s, Node.apply(s), End), Node(s, End, Node.apply(s)))
    case n if n % 2 == 0 => {
      val left = cBalanced(n / 2, s)
      val right = cBalanced(n / 2 - 1, s)
      balance(s, left, right).distinct
    }
    case n if n % 2 != 0 => {
      val left = cBalanced(n / 2, s)
      balance(s, left, left).distinct
    }
  }

  // Scala 99 version
  // clever use of flatmap, which solves my issue above with pairs getting generated twice

  def tBalanced[T](n: Int, value: T): List[Tree[T]] = n match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => {
      val subtrees = tBalanced(n / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    }
    case n if n % 2 == 0 => {
      val lesserSubtrees = tBalanced((n - 1) / 2, value)
      val greaterSubtrees = tBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
    }
  }


  def main(args: Array[String]): Unit = {
    //    println(cBalanced(4, "x"))
    println("Elapsed time of my solution: ")
    println(Timer.timeFunction(cBalanced(4, "x")))

    //    println(tBalanced(4, "x"))
    println("Elapsed time of scala99 solution: ")
    println(Timer.timeFunction(tBalanced(4, "x")))

    // conclusion: my version is significantly slower than the scala99 solution, as expected cause of the trees getting generated multiple times
  }


}
