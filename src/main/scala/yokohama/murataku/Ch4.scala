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

  object Option {
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case (_, _) => None
    }

    def sequence[A](list: List[Option[A]]): Option[List[A]] = list match {
      case Nil => Some(Nil)
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  val none: Option[Int] = None

  println("map for some: " + Some(3).map(a => a.toDouble / 2.0))
  println("map for none: " + none.map(a => a.toDouble / 2.0))

  println("flatMap for some: " + Some(3).flatMap(a => Some(a.toDouble / 2.0)))
  println("flatMap for none: " + none.flatMap(a => Some(a.toDouble / 2.0)))

  println("map2 Some vs Some: " + Option.map2(Some(1), Some(5))(_ + _) )
  println("map2 None vs Some: " + Option.map2(none, Some(5))((a,b)=>a+b) )
  println("map2 None vs None: " + Option.map2(none, none)((a,b)=>a+b) )

  println("sequence for Some: " + Option.sequence( List(Some(1), Some(5)) ) )
  println("sequence for Some and None: " + Option.sequence( List(Some(1), None, Some(5)) ) )
}
