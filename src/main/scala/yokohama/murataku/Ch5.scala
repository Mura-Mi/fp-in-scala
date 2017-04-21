package yokohama.murataku
object Ch5 extends App {
  trait Stream[+A]
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
      lazy val h = head
      lazy val t = tail
      Cons(() => h, () => t);
    }

    def empty[A]: Stream[A] = Empty
  }
}
