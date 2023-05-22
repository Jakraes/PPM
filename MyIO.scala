import Cells.{Board, Red}

import java.io._
import scala.io.Source

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

  // TODO NAO FUNCIONA
  def saveGame(turn: Int, board: Board, rand: MyRandom) = {
    def boardToString(l: List[Cells.Cell]): String = l match {
      case Nil => ""
      case x::xs => {
        x match {
          case Cells.Red => "X" + boardToString(xs)
          case Cells.Blue => "O" + boardToString(xs)
        }
      }
    }

    val pw = new PrintWriter(new File("save.bin"))
    pw.write(turn)
    pw.write("\n")
    pw.write(board.size)
    pw.write("\n")
    pw.write(boardToString(Utils.matrixToList(board)))
    pw.write("\n")
    pw.write(rand.seed.toInt)
    pw.write("\n")
    pw.close()
  }

  def loadGame(): (Int, Board, MyRandom) = {
    def playFromString(s: String, game: Game, board: Board, accX: Int = 0, accY: Int = 0): Board = accY match {
      case game.size => board
      case _ => {
        val cell: Cells.Cell = if (s.charAt(accY * game.size + accX).equals('X')) Cells.Red else Cells.Blue
        val newBoard = game.play(board, cell, accX, accY)

        if (accX + 1 == game.size) playFromString(s, game, board, 0, accY + 1)
        else playFromString(s, game, board, accX + 1, accY)
      }
    }

    val source = Source.fromFile("save.bin")
    var l: List[String] = List()  // Tem que ser var por causa do for

    for (line <- source.getLines) {
      l = l :+ line
    }

    val turn = l(0).toInt
    val size = l(1).toInt
    val cells = l(2)
    val seed = l(3).toLong

    val game = new Game(new MyRandom(0), size)
    val board = playFromString(cells, game, game.board)

    (turn, board, new MyRandom(seed))
  }
}
