package yokohama.murataku

trait Monoid[A] {
  def op(left: A, right: A): A
  def zero: A
}

object MonoidApp extends App {
  val sm = new Monoid[String] {
    def op(a1: String, a2: String) = a1+a2
    def zero = ""
  }

  val addition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1+a2
    def zero = 0
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero = None
  }

  val om: Monoid[Option[String]] = optionMonoid
  println("om: " + om.op(Some("One"), Some("Two")))
  println("om: " + om.op(None, Some("Two")))
  println("")
  println("om: " + om.op(Some("One"), om.op(Some("Two"), Some("Three"))))
  println("om: " + om.op(om.op(Some("One"), Some("Two")), Some("Three")))

  println("---")
  
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(left: A => A, right: A => A) = left andThen right
    def zero: A=>A = a=>a
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A=>B): B = as match {
    case Nil => m.zero
    case head :: Nil => f(head)
    case head :: rest => m.op(f(head), foldMap(rest, m)(f))
  }

  println("foldMap: "+foldMap(List("This", "is", "a", "pen"), addition)(_.length))

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A=>B): B = ???
}
