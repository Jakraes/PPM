import Utils.createList
import scala.annotation.tailrec
import Cells.Board

case class Game(size: Int) {
  val defaultBoard: Board = createList(size, createList(size, Cells.Empty))


  def isValidMove(board: Board, x: Int, y: Int) = Game.isValidMove(board, x, y)

  def randomMove(board: Board, rand: MyRandom) = Game.randomMove(board,rand)

  def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = Game.play(board,player,row,col)

  def displayBoard(board: Board) = Game.displayBoard(board)

  def hasContiguousLine(board: Board, player: Cells.Cell) = Game.hasContiguousLine(board, player)

  def start = Game.start(this.defaultBoard)


  //TODO: the one bellow might need cleaning
  def loop(board: Board , turn: Int = 0, rand: MyRandom, input: MyInput) = Game.loop(this.defaultBoard, turn, rand, input)

}

//TODO: tirar privates?


object Game{
  /* Função auxiliar que avalia se o jogador fez uma jogada válida
     *
     * @param board Tabuleiro do jogo
     * @param x     Coordenada X da jogada
     * @param y     Coordenada Y da jogada
     * @return      True se a jogada é válida, False caso contrário
     */
  private def isValidMove(board: Board, x: Int, y: Int): Boolean = {
    //Range(0, size).contains(x) && Range(0, size).contains(y) && board(y)(x) == Cells.Empty
    val l_temp = 0 until board.size
   ( l_temp contains x) && (l_temp contains y) && board(y)(x) == Cells.Empty
  }

  // T1
  @tailrec
  private def randomMove(board: Board, rand: MyRandom): ((Int, Int), MyRandom) = {
    val (x, rand2) = rand.nextInt(board.size)
    val (y, rand3) = rand2.nextInt(board.size)
    board(y)(x) match {
      case Cells.Empty => ((x, y), rand3)
      case _ => randomMove(board, rand3)
    }
  }

  // T2
  private def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = {

    board updated(col, board(col) updated(row, player))
  }

  // T3
  private def displayBoard(board: Board) = {
    def aux(board: Board, acc: Int = 0): String = board match {
      case Nil => ""
      case x :: xs => " " * acc + Cells.Red + " " + (x foldRight "")(_ + " " + _) + Cells.Red + "\n" + aux(xs, acc + 1)
    }

    println("< -" + " - " * board.size + "- >")
    println((" " + Cells.Blue) * board.size)
    print(aux(board))
    println(" " * (board.size + 1) + (" " + Cells.Blue) * board.size)
  }

  // TODO T4
  def hasContiguousLine(board: Board, player: Cells.Cell) = {
    val cells = Utils.getIndexInMatrix(board, player)
  }

  // TODO T5
  // def undo(???)

  private def loop(board: Board, turn: Int = 0, rand: MyRandom, input: MyInput) {
    if (turn % 2 == 0) { // Turno do jogador
      println("1. Fazer jogada \nQ. Abandonar jogo")
      val opt = input.getLine

      opt match {
        case "1" => {
          print("X: ")
          val x = input.getInt
          print("Y: ")
          val y = input.getInt

          if (isValidMove(board, x, y)) {
            val newBoard = play(board, Cells.Blue, x, y)
            displayBoard(newBoard)
            loop(newBoard, turn + 1, rand, input)
          }
          else {
            println("Invalid move")
            loop(board, turn, rand, input)
          }
        }
        case "Q" | "q" => {}
        case _ => {
          loop(board, turn, rand, input)
        }
      }
    }

    else { // Turno do CPU
      val ((x, y), newRand) = randomMove(board, rand)
      val newBoard = play(board, Cells.Red, x, y)
      displayBoard(newBoard)
      loop(newBoard, turn + 1, newRand, input)
    }
  }

  def start(board: Board) {
    val rand = MyRandom(0)
    val input = MyInput()
    displayBoard(board)
    loop(board, rand = rand, input = input)
  }
}