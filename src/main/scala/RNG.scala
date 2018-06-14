trait RNG {
  def nextInt: (Int, RNG)
}

trait Rand[+A] extends Function1[RNG, (A, RNG)] {
  def flatMap[B](g: A => Rand[B]): Rand[B] = RNG.flatMap(this)(g)
  def map[B](f: A => B): Rand[B] = RNG.map(this)(f)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng1 => {
    val (a, rng2) = f(rng1)
    g(a)(rng2)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng1 => {
    val (a, rng2) = s(rng1)
    (f(a), rng2)
  }
}
