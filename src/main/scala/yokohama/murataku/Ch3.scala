package yokohama.murataku;

object Ch3 extends App {
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
    }

    def tail[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(a, b) => b
    }

    def setHead[A](list: List[A], v: A): List[A] = list match {
      case Nil => Nil
      case Cons(a, b) => Cons(v, b)
    }

    def drop[A](l: List[A], n: Int): List[A] = n match {
      case 0 => l
      case 1 => List.tail(l)
      case a => List.drop(List.tail(l), a-1)
    }

    def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(a,b) if p(a) => List.dropWhile(tail(l), p)
      case _ => l
    }

    def sum(ints: List[Int], b: Int): Int = foldRight(ints, b)((x: Int, y: Int) => x+y)
    
    /*
    ints match {
      case Nil => b
      case Cons(a, list) => sum(list, a+b)
    }
    */

    def sum(ints: List[Int]): Int = sum(ints, 0)

    def product(ints: List[Int]): Int = product(ints, 1)

    def product(ints: List[Int], b: Int): Int = foldRight(ints, 1)(_ * _)
    
    /*
    ints match {
      case Nil => b
      case Cons(a, rest) => product(rest, a * b)
    }
    */

    def foldRight[A, B](vals: List[A], b: B)(f: (A, B) => B): B = vals match {
      case Nil => b
      case Cons(x, xs) => f(x, foldRight(xs, b)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def length[A](as: List[A]): Int = foldRight(as, 0)((a, b) => {b + 1})
    def lengthL[A](as: List[A]): Int = foldLeft(as, 0)((a, b) => {a + 1})

    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc, a) => Cons(a, acc))

    def map[A, B](l: List[A])(m: A=>B): List[B] = 
      foldRight(l, Nil: List[B])((a, b) => Cons(m(a), b))
  }

  println(List.dropWhile(List(2,4,6,8,7,5,6), (i: Int) => i < 5))
  println("sum:" + List.sum(List(1,2,3,4,5)))
  println("product:" + List.product(List(1,2,3,4,5)))
  println("product0:" + List.product(List(1,2,0,4,5)))
  println("length: " + List.length(List(1,2,0,4,5)))
  println("length: " + List.lengthL(List(1,2,0,4,5)))
  println("reverse: " + List.reverse(List(1,2,3,4,5)))

  println("divide by map: " + List.map(List(1,2,3,4,5))((a: Int) => a.toDouble / 2d))
}
