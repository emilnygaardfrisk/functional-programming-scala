import scala.annotation.tailrec
object Chapter2 extends App {
  // 2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, current: Int): Int = {
      if (n < 2) current
      else go(n-1, current, prev + current)
    }

    go(n, 0, 1)
  }
  println(fib(19))

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(sorted: Boolean, i: Int): Boolean = {
      if (i == as.length - 1 || !sorted) sorted
      else go(ordered(as(i), as(i + 1)), i + 1)
    }

    go(true, 0)
  }
  println(isSorted(Array(1, 2, 3, 4), (x: Int,  y: Int) => x < y))
  println(isSorted(Array(4, 3, 2, 1), (x: Int,  y: Int) => x < y))

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
    a => b => f(a, b)

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
