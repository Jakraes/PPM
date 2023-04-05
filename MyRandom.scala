case class MyRandom(seed: Long) {
  def nextInt: (Int, MyRandom) = MyRandom.nextInt(this.seed)
  def nextInt(n: Int) = MyRandom.nextInt(this.seed,n)

}
object MyRandom{

  //TODO: CLEAN
  def nextInt(seed:Long): (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRandom)
  }

  def nextInt(seed:Long,n: Int): (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val nn = ((newSeed >>> 16).toInt) % n
    (if (nn < 0) -nn else nn, nextRandom)
  }
}