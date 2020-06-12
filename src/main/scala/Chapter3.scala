object Chapter3 extends App {
  // 3.1
  val x = 1 + 2

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => Cons(newHead, Nil)
    case Cons(h, t) => Cons(newHead, t)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  // 3.6
  def init[A](l: List[A]): List[A] = {
    def go(in: List[A], res: List[A]): List[A] = in match {
      case Nil => res
      case Cons(h, Nil) => res
      case Cons(h, t) => go(t, append(res, Cons(h, Nil)))
    }

    go(l, Nil)
  }


  // [1, 2]
  // [2,3,4]
  // 1, [2,3,4]
  // 2, [3,4]
  
  
}
