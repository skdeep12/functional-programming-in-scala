trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n,nextRNG)
  }
}
type Rand[+A] = RNG => (A, RNG)
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (i,nextRNG) = rng.nextInt
  (if (i < 0) -(i+1) else i, nextRNG)
}

def double(rng: RNG): (Double, RNG) = {
  val (i,nextRNG) = nonNegativeInt(rng)
  (i.toDouble/(Int.MaxValue.toDouble+1), nextRNG)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
  case 0 => (Nil,rng)
  case _ => val (l,newRNG) = ints(count-1)(rng)
            val (n, _rng ) = newRNG.nextInt
            (n::l,_rng)
}