import Cells.Board

case class MyRandom(seed: Long) {
  //TODO
  def nextInt: (Int, MyRandom) = MyRandom.nextInt(this.seed)

  def nextInt(n: Int) = MyRandom.nextInt(this.seed, n)

  def randomMove(board: Board, rand: MyRandom): ((Int, Int), MyRandom) = MyRandom.randomMove(board, rand)
}

object MyRandom {
  def nextInt(seed: Long, n: Int): (Int, MyRandom) = {
    val (nn, nextR) = nextInt(seed)
    ((nn % n).abs, nextR)
  }

  def nextInt(seed: Long): (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRandom)
  }

  // T1
  def randomMove(board: Board, rand: MyRandom): ((Int, Int), MyRandom) = {
    val casasVazias = Utils.getIndexInMatrix(board, Cells.Empty)
    val (i, rand2) = rand.nextInt(casasVazias.size)
    (casasVazias(i), rand2)
  }
}