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

    def dropWhileCurry[A](l: List[A])(p: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(a,b) if p(a) => List.dropWhileCurry(tail(l))(p)
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

    def head[A](l: List[A], count: Int):List[A] = (l, count) match {
      case (_, n) if n < 0 => Nil
      case (Nil, _) => Nil
      case (Cons(a, b), 0) => Nil
      case (Cons(a, b), n) => Cons(a, head(b, n-1))
    }

    def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (a, b) if length(a) < length(b) => false
      case (Cons(x, xs), Cons(y, ys)) if x != y => hasSubSequence(xs, sub)
      case (Cons(x, _), Cons(y, Nil)) => x == y
      case (Cons(x, xs), Cons(y, ys)) if x == y => same(xs, head(ys, length(xs))) || hasSubSequence(xs, sub)
      case (Cons(x, xs), _) => hasSubSequence(xs, sub)
    }

    def same[A](a: List[A], b: List[A]): Boolean = (a, b) match {
      case (Cons(x, Nil), Cons(y, Nil)) => true
      case (Nil, _) => false
      case (_, Nil) => false
      case (Cons(x, xs), Cons(y, ys)) if x != y => false
      case (Cons(x, xs), Cons(y, ys)) if x == y => same(xs, ys)
    }
  }

  println(List.dropWhile(List(2,4,6,8,7,5,6), (i: Int) => i < 5))
  println(List.dropWhileCurry(List(2,4,6,8,7,5,6))(i => i < 5))
  println("sum:" + List.sum(List(1,2,3,4,5)))
  println("product:" + List.product(List(1,2,3,4,5)))
  println("product0:" + List.product(List(1,2,0,4,5)))
  println("length: " + List.length(List(1,2,0,4,5)))
  println("length: " + List.lengthL(List(1,2,0,4,5)))
  println("reverse: " + List.reverse(List(1,2,3,4,5)))

  println("divide by map: " + List.map(List(1,2,3,4,5))((a: Int) => a.toDouble / 2d))

  println("head: " + List.head(List(1,2,3,4,5), 2))
  println("head: " + List.head(List(1,2,3,4,5), 8))

  println("same false: " + List.same(List(1,2,3,4,5), List(2,3,4)))
  println("same true: " + List.same(List(2,3,4), List(2,3,4)))

  println("subseq true: " + List.hasSubSequence(List(1), List(1)))
  println("subseq true: " + List.hasSubSequence(List(1,2), List(1)))
  println("subseq true: " + List.hasSubSequence(List(1,2), List(1,2)))
  println("subseq true: " + List.hasSubSequence(List(1,2,3), List(1,2)))
  println("subseq true: " + List.hasSubSequence(List(2,3), List.head(List(2), 2)))
  println("subseq true: " + List.hasSubSequence(List(1,2,3,4,5), List(2,3,4)))
}
