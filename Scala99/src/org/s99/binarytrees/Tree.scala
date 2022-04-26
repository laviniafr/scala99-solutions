package org.s99.binarytrees

import org.s99.arithmetics.Timer

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {

  /*
    Function covering all possible left and right pairings.
   */
  def bal(s: String, left: List[Node[String]], right: List[Node[String]]): List[Node[String]] = (s, left, right) match {
    case (s, h :: Nil, j :: Nil) => List(Node(s, h, j), Node(s, j, h))
    case (s, h :: Nil, j :: jail) => Node(s, h, j) :: Node(s, j, h) :: bal(s, List(h), jail)
    case (s, h :: tail, j :: Nil) => Node(s, h, j) :: Node(s, j, h) :: bal(s, tail, List(j))
    case (s, h :: tail, j :: jail) => Node(s, h, j) :: Node(s, j, h) :: bal(s, tail, jail) ::: bal(s, List(h), jail) ::: bal(s, List(j), tail)
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
      bal(s, left, right).distinct
    }
    case n if n % 2 != 0 => {
      val left = cBalanced(n / 2, s)
      bal(s, left, left).distinct
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
    case n if n % 2 ==0 => {
      val lesserSubtrees = tBalanced((n-1)/2, value)
      val greaterSubtrees = tBalanced((n-1)/2 +1, value)
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