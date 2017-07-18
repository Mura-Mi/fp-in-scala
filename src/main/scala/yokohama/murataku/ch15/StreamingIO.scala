package yokohama.murataku.ch15

sealed trait Process[I,O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream.empty
    case Await(recv) => s match {
      case h #:: rest => recv(Some(h))(rest)
      case other => recv(None)(other)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I,O] = {
    def go(p: Process[I,O]): Process[I,O] = p match {
      case Halt() => go(this)
      case Emit(h,t) => Emit(h, go(t))
      case Await(recv) => Await {
        case None => recv(None)
        case some => go(recv(some))
      }
    }
    go(this)
  }

  def |>[O2](p2: Process[O, O2]): Process[I, O2] =
    p2 match {
      case Halt() => Halt()
      case Emit(h,t) => Emit(h, this |> t)
      case Await(recv) => 
        this match {
          case Halt() => Halt()
          case Emit(h, t) => t |> recv(Some(h))
          case Await(rr) => Await((i: Option[I]) => rr(i) |> p2)
        }
    }

  def map[O2](f: O => O2): Process[I, O2] = this |> Process.lift(f)

  def ++(p: => Process[I,O]): Process[I,O] = this match {
    case Halt() => p
    case Emit(h,t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_.flatMap(f)))
  }

}

case class Emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]
case class Halt[I,O]() extends Process[I,O]
case class Await[I,O](recv: Option[I] => Process[I, O]) extends Process[I,O]

object Process {
  def count[I]: Process[I, Int] = {
    def countUp(n: Int): Process[I,Int] = await(_ => Emit(n,countUp(n+1)))
    countUp(1)
  }

  def countLoop[I]: Process[I, Int] = loop(0)((input:I, cnt:Int) => (cnt+1,cnt+1))
  def sumLoop: Process[Double, Double] = loop(0.0)((e, s) => (e+s, e+s))

  def mean: Process[Double, Double] = ???

  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] = 
    await((i: I) => f(i,z) match {
      case (o,s2) => Emit(o, loop(s2)(f))
    })

  def await[I,O](proc: I => Process[I,O]): Process[I,O] =
    Await {
      case None => Halt()
      case Some(i) => proc(i)
    }


  def liftOne[I,O](f: I => O): Process[I,O] = await(i => Emit(f(i)))
  /* 下記と同じ
  Await[I,O] {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }
  */

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat

  def take[I](n: Int): Process[I,I] = 
    if (n > 0) await(i => Emit(i, take(n-1)))
    else Halt()

  def drop[I](n: Int): Process[I,I] = 
    if (n > 0) await(i => drop(n-1))
    else lift[I,I](a => a)

  def monad[I]: Monad[({type f[x] = Process[I, x]})#f] = 
    new Monad[({type f[x] = Process[I, x]})#f] {
      def unit[O](o: => O): Process[I, O] = Emit(o)
      def flatMap[O,O2](p: Process[I,O])(f: O => Process[I, O2]): Process[I, O2] = p.flatMap(f)
    }

}

object Runner extends App {
  println("fp in scala is almost done!!!")

  val nums = Stream(5,2,7,23,10,26,1,8,22)
  val empty = Stream.empty

  println("");println("liftOne の例")
  val sqrt: Int => Double = i => Math.sqrt(i.toDouble)
  val sLifted: Process[Int, Double] = Process.liftOne(sqrt)

  println("Empty に singleLifted をくっつけてもempty : " + sLifted(empty)) // => empty

  println("liftOne だと先頭にしか関数が適用されない") 
  sLifted(nums).foreach(println) // => Stream(2.236)

  // liftOne に 要素が 2 個以上ある Stream を渡したときに何が起きている？ => ひたすらインライン展開してみた
  sLifted(nums)

  Await[Int, Double]({
    case Some(i) => Emit[Int, Double](sqrt(i))
    case None => Halt[Int, Double]()
  })(Stream(5,2,7,23,10,26,1,8,22))

  Stream(5,2,7,23,10,26,1,8,22) match {
    case h #:: rest => Emit[Int, Double](sqrt(h))(rest)
    // case other => recv(None)(other)
  } // => Emit(2.23......, Halt) apply rest
    // => 2.23 #:: Halt.apply(rest)
    // => 2.23 #:: Stream.empty

  // 頑張ったインライン展開ここまで

  println("");println("repeat の例")
  
  val liftfunc: Process[Int, Double] = Process.lift(sqrt)
  val lifted: Stream[Double] = liftfunc(nums)
  println("lift 関数を適用したら Stream の要素全部に適用される！")
  lifted.foreach(println)
  
  println("")
  println("take")
  Process.take(3)(nums).foreach(println)

  println("")
  println("drop")
  Process.drop(6)(nums).foreach(println)

  println("")
  println("count")
  Process.count(nums).foreach(println)

  /*
  println("")
  println("mean")
  Process.mean(nums.map(_.toDouble)).foreach(println)
  */

  println("")
  println("countLoop")
  Process.countLoop(nums).foreach(println)

  println("")
  println("sumLoop")
  Process.sumLoop(nums.map(_.toDouble)).foreach(println)

  println("")
  println("|>")
  val sqrt3times: Process[Int, Double] = Process.lift(sqrt) |> Process.lift(_ + 1.0)
  sqrt3times(nums).foreach(println)

  println("")
  println("++")
  val twice: Process[Int,Double] = Process.take(3).map(_ * 2.0)
  val plusTen: Process[Int,Double] = Process.lift(_ + 10.0)
  (twice ++ plusTen)(nums) foreach println

  println("")
  println("flatMap")




}

/** Monad / Functor **/

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}
trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](fa: M[A])(f: A => B): M[B] = flatMap(fa)(a => unit(f(a)))
}
