import Cells.Board

object MyIO { //TODO object como ele disse
  def getLine: String = scala.io.StdIn.readLine

  def getInt: Int = scala.io.StdIn.readInt

  // T3
  def displayBoard(board: Board) = {
    val input = MyIO

    def aux(board: Board, acc: Int = 0): String = board match {
      case Nil => ""
      case x :: xs => " " * acc + Cells.Red + " " + (x foldRight "")(_ + " " + _) + Cells.Red + "\n" + aux(xs, acc + 1)
    }

    input.println("< -" + " - " * board.size + "- >")
    input.println((" " + Cells.Blue) * board.size)
    input.print(aux(board))
    input.println(" " * (board.size + 1) + (" " + Cells.Blue) * board.size)
  }

  def print(s: Any) = scala.Predef.print(s)

  def println(s: Any) = scala.Predef.println(s)

  // TODO print menu, get char, ...
}
