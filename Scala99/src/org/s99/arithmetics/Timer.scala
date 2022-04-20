package org.s99.arithmetics

/*
  Function to time given block of code.
 */
object Timer {

  def timeFunction[T](f: => T): Long = {
    val start = System.nanoTime()
    f
    val end = System.nanoTime()
    end - start
  }

}
