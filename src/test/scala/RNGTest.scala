object RNGTest extends App {
  val xy1: Rand[Int] = for {
    x <- RNG.int
    y <- RNG.int
  } yield x + y

  val xy2: Rand[Int] = RNG.int.flatMap { x =>
    RNG.int.map { y =>
      x + y
    }
  }

  val rng = RNG.Simple(42)
  println(xy1(rng)._1)
  println(xy2(rng)._1)
}
