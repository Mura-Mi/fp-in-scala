package yokohama.murataku

object State extends App {
  def next(rnd: RNG): RNG = {rnd.nextInt._2}

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n: Int = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

  }

  val (fInt, fRand) = SimpleRNG(42).nextInt
  println("nextInt: " + fInt)
  val (sInt, sRand) = fRand.nextInt
  println("nextInt: " + sInt)
  val r = sRand.nextInt._2.nextInt
  println("nextNextInt: " + r._1)

  def nextIntPair(rand: RNG): ((Int, Int), RNG) = {
    val (first, nRand) = rand.nextInt
    val (second, secRand) = nRand.nextInt
    ((first, second), secRand)
  }

  val r2 = nextIntPair(r._2)
  println(r2)
  val nn = r2._2

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i > 0) (i, r) else (-(i+1), r)
  }

  val r3 = nn.nextInt._2.nextInt._2
  println("nonNegativeInt: " + nonNegativeInt(r3)._1)

  def double(rng: RNG): (Double, RNG) = {
    val (a, b) = rng.nextInt
    ((Math.abs(a) - 1).toDouble / Int.MaxValue, b)
  }
  
  val r4 = r3.nextInt._2
  println("double: " + double(r4.nextInt._2.nextInt._2))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def ints(n: Int, list: List[Int])(rng: RNG): (List[Int], RNG) = n match {
      case nu if nu < 1 => (list, rng)
      case nu => ints(nu - 1, (rng.nextInt._1 :: list))(rng.nextInt._2)
    } 
    ints(count, Nil)(rng)
  }

  val r5 = r4.nextInt._2
  println("ints1 :" + ints(10)(r5)._1)
  val r6 = r5.nextInt._2
  println("ints2 :" + ints(10)(r6)._1)

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  val r7 = next(r6)
  println("map: " + map(nonNegativeInt)(a => "hogehoge: " + a.toString + ": fugafuga")(r7)._1)

  def doubleByMap(rng: RNG): (Double, RNG) = 
    map(int)(i => (Math.abs(i) -1).toDouble / Int.MaxValue)(rng)
  val r8 = next(r7)
  println("doubleByMap: " + doubleByMap(r8)._1)
  println("doubleByMap: " + doubleByMap(r8.nextInt._2)._1)
  println("doubleByMap: " + doubleByMap(r8.nextInt._2.nextInt._2)._1)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B)=>C): Rand[C] = { rnd =>
    val (aVal, aRand) = ra(rnd)
    val (bVal, bRand) = rb(aRand)
    (f(aVal, bVal), bRand)
  }

  /*
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng {
    fs match {
      case rndlet :: Nil => {
          val (a, rng2) = rndlet(rng)
          (List(rndlet), rng2)
        }
      case rndlet :: rest => {
          val ()
        }
    }
  }
  */
}
