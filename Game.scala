import scala.annotation.tailrec

case class Game(size: Int) {
  type Board = List[List[Cells.Cell]]

  // T1
  def randomMove(board: Board, rand: MyRandom): ((Int, Int), MyRandom) = {
    val (x, rand2) = rand.nextInt(size)
    val (y, rand3) = rand2.nextInt(size)
    board(y)(x) match {
      case Cells.Empty => ((x, y), rand3)
      case _ => randomMove(board, rand3)
    }
  }

  // T2
  def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = {
    board updated(col, board(col) updated(row, player))
  }

  // T3
  def displayBoard(board: Board) = {
    def aux(board: Board, acc: Int = 0): String = board match {
      case Nil => ""
      case x::xs => " " * acc + Cells.Blue + " " + (x foldRight "") (_ + " " + _) + Cells.Blue + "\n" + aux(xs, acc + 1)
    }

    println((" " + Cells.Red) * size)
    print(aux(board))
    println(" " * (size + 1) + (" " + Cells.Red) * size)
  }

  // T4
  // def hasContiguousLine(board: Board)

  // T5
  // def undo(???)
}
