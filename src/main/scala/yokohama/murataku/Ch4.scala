package yokohama.murataku

object Ch4 extends App {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](ob: => B): B = this match {
      case None => ob
      case Some(a) => a
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  val none: Option[Int] = None

  println("map for some: " + Some(3).map(a => a.toDouble / 2.0))
  println("map for none: " + none.map(a => a.toDouble / 2.0))

  println("flatMap for some: " + Some(3).flatMap(a => Some(a.toDouble / 2.0)))
  println("flatMap for none: " + none.flatMap(a => Some(a.toDouble / 2.0)))
}
