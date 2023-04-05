import Utils.createList
import scala.annotation.tailrec
import Cells.Board

case class Game(rand: MyRandom, input: MyInput) {
  def isValidMove(board: Board, x: Int, y: Int) = Game.isValidMove(board, x, y)

  def randomMove(board: Board) = Game.randomMove(board,rand)

  def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = Game.play(board, player, row, col)

  def displayBoard(board: Board) = Game.displayBoard(board)

  def hasContiguousLine(board: Board, player: Cells.Cell) = Game.hasContiguousLine(board, player)

  def start = Game.start(rand, input)


  def menuLoop(size: Int = 0, diff: Int = 0) = Game.menuLoop(rand, input, size, diff)

  // TODO: the one bellow might need cleaning
  // Concordo, ainda precisa aí de um quality check - João C
  def loop(board: Board , turn: Int = 0) = Game.gameLoop(board, turn, rand, input)

}

// TODO: tirar privates?
// Se achares melhor então ya - João C


object Game{
  private def createBoard(size: Int): Board = createList(size, createList(size, Cells.Empty))

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
    (l_temp contains x) && (l_temp contains y) && board(y)(x) == Cells.Empty
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

  // TODO: Arranjar maneira desta função funcionar sem dar return a Any
  private def menuLoop(rand: MyRandom, input: MyInput, size: Int = 5, diff: Int = 0): Any = {
    print(s"${Console.RESET}1. Começar jogo \n2. Configurar jogo \nQ. Sair\n> ")
    val i = input.getLine

    i match {
      case "1" => {
        val newBoard = createBoard(size)
        displayBoard(newBoard)
        gameLoop(newBoard, rand = rand, input = input)
      }
      case "2" => {
        def menuConfig(rand: MyRandom, input: MyInput, size: Int = 0, diff: Int = 0): Any = {
          print(s"${Console.RESET}1. Mudar tamanho do tabuleiro \n2. Mudar dificuldade \nB. Voltar \n> ")
          val j = input.getLine
          j match {
            case "1" => {
              print(s"${Console.RESET}Tamanho: ")
              val newSize = input.getInt
              menuConfig(rand, input, newSize, diff)
            }
            case "2" => {
              print(s"${Console.RESET}Dificuldade: ")
              val newDiff = input.getInt
              menuConfig(rand, input, size, newDiff)
            }
            case "B" | "b" => menuLoop(rand, input, size, diff)
            case _ => menuConfig(rand, input, size, diff)
          }
        }

        menuConfig(rand, input, size, diff)
      }
      case "Q" | "q" => {}
      case _ => menuLoop(rand, input, size, diff)
    }
  }

  private def gameLoop(board: Board, turn: Int = 0, rand: MyRandom, input: MyInput) {
    if (turn % 2 == 0) { // Turno do jogador
      print(s"${Console.RESET}1. Fazer jogada \nQ. Abandonar jogo \n> ")
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
            gameLoop(newBoard, turn + 1, rand, input)
          }
          else {
            println("Invalid move")
            gameLoop(board, turn, rand, input)
          }
        }
        case "Q" | "q" => menuLoop(rand, input, board.size)
        case _ => {
          gameLoop(board, turn, rand, input)
        }
      }
    }

    else { // Turno do CPU
      val ((x, y), newRand) = randomMove(board, rand)
      val newBoard = play(board, Cells.Red, x, y)
      displayBoard(newBoard)
      gameLoop(newBoard, turn + 1, newRand, input)
    }
  }

  def start(rand: MyRandom, input: MyInput) {
    menuLoop(rand, input)
  }
}