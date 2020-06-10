object Chapter3 extends App {
  // 3.1
  val x = 1 + 2

  // 3.2 Ikke rigtigt implementeret
  def tail[A](l: List[A]): List[A] =
    l.tail
}
