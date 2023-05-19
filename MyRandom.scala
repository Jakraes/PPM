import Cells.Board

import scala.annotation.tailrec

case class MyRandom(seed: Long) {
  //TODO
  def nextInt: (Int, MyRandom) = MyRandom.nextInt(this.seed)
  def nextInt(n: Int) = MyRandom.nextInt(this.seed,n)
  def randomMove(board: Board, rand: MyRandom): ((Int, Int), MyRandom) = MyRandom.randomMove(board, rand)
}
object MyRandom{
  def nextInt(seed:Long): (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRandom)
  }

  def nextInt(seed:Long,n: Int): (Int, MyRandom) = {
    val (nn,nextR) =  nextInt(seed)
    ((nn % n ).abs, nextR)
  }

  @tailrec
  def randomMove(board: Board, rand: MyRandom): ((Int, Int), MyRandom) = {
    val (x, rand2) = rand.nextInt(board.size)
    val (y, rand3) = rand2.nextInt(board.size)
    board(y)(x) match {
      case Cells.Empty => ((x, y), rand3)
      case _ => randomMove(board, rand3)
    }
  }
}