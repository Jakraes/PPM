import Cells.Board
import Utils.{createList, hasContiguousLine, isValidMove}

case class Game(rand: MyRandom) {
  val input = MyIO
  def isValidMove(board: Board, x: Int, y: Int) = Utils.isValidMove(board, x, y)

  //TODO corrigir
  def randomMove(board: Board) = rand.randomMove(board,rand)

  def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = Game.play(board, player, row, col)

  def displayBoard(board: Board) = input.displayBoard(board) //TODO corrigir

  //TODO corrigir
  //def hasContiguousLine(board: Board, player: Cells.Cell) = Utils.hasContiguousLine(board, player)

  def start = Game.start(rand)

  def menuLoop(size: Int = 0, diff: Int = 0) = Game.menuLoop(rand,  size, diff)

  // TODO: the one bellow might need cleaning
  // Concordo, ainda precisa aí de um quality check - João C
  def gameLoop(board: Board , turn: Int = 0) = Game.gameLoop(board, turn, rand, board, turn)
}

object Game{
  private def createBoard(size: Int): Board = createList(size, createList(size, Cells.Empty))
  /* Função auxiliar que avalia se o jogador fez uma jogada válida
     *
     * @param board Tabuleiro do jogo
     * @param x     Coordenada X da jogada
     * @param y     Coordenada Y da jogada
     * @return      True se a jogada é válida, False caso contrário
     */


  // T2
  private def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = {
    board updated(col, board(col) updated(row, player))
  }

  // TODO T6
  // Arranjar maneira desta função funcionar sem dar return a Any
  private def menuLoop(rand: MyRandom, size: Int = 5, diff: Int = 0): Any = {
    val input = MyIO
    MyIO.print(s"${Console.RESET}Menu principal:\n")

    input.print(s"${Console.RESET}\t1: Novo jogo \n\t2: Configurar jogo \n\tQualquer tecla: Sair\n> ")
    val i = input.getLine

    i match {
      case "1" => {
        comecarJogo(rand, size, input)
      }
      case "2" => {
        menuConfig(rand,  size, diff)
      }
      case _ => {}
    }
  }

  private def comecarJogo(rand: MyRandom, size: Int, input: MyIO.type): Unit = {
    val newBoard = createBoard(size)
    input.displayBoard(newBoard)
    gameLoop(newBoard, rand = rand, oldBoard = newBoard, oldTurn = 0)
  }

  def menuConfig(rand: MyRandom, size: Int = 0, diff: Int = 0): Any = {
    MyIO.print(s"${Console.RESET}Menu de configurações:\n")
    MyIO.print(s"${Console.RESET}\tClique em:\n\t\t1: Mudar tamanho do tabuleiro \n\t\t2: Mudar dificuldade \n\t\tQualquer tecla: Voltar \n> ")
    val j = MyIO.getLine
    j match {
      case "1" => {
        MyIO.print(s"${Console.RESET}Tamanho: ")
        val newSize = MyIO.getInt
        menuConfig(rand, newSize, diff)
      }
      case "2" => {
        MyIO.print(s"${Console.RESET}Dificuldade: ")
        val newDiff = MyIO.getInt
        menuConfig(rand, size, newDiff)
      }
      case _ => menuLoop(rand, size, diff)
    }
  }

  private def gameLoop(board: Board, turn: Int = 0, rand: MyRandom, oldBoard: Board, oldTurn: Int) {

    def playerTurn(): Unit = {
      // Turno do jogador
      MyIO.print(s"${Console.RESET}\tClique em:\n\t1. Fazer jogada. \n\t2. Undo. \n\tQualquer tecla. Abandonar jogo. \n> ")
      val opt = MyIO.getLine

      opt match {
        case "1" => {
          MyIO.print("X = ")
          val x = MyIO.getInt
          MyIO.print("Y = ")
          val y = MyIO.getInt

          if (isValidMove(board, x, y)) {
            //TODO melhorar

            val oldBoard = board
            val newBoard = play(board, Cells.Blue, x, y)
            MyIO.displayBoard(newBoard)
            if (hasContiguousLine(newBoard, Cells.Blue))
              MyIO println("Tu vences-te!! :)")
            else
              gameLoop(newBoard, turn + 1, rand, oldBoard, turn) // T5
          }
          else {
            MyIO println("Jogada invalida")
            gameLoop(board, turn, rand, oldBoard, oldTurn) // T5
          }
        }
        case "2" => {
          MyIO displayBoard (oldBoard)
          gameLoop(oldBoard, oldTurn, rand, oldBoard, oldTurn) // T5
        }
        case _ => {
          menuLoop(rand, board.size)
        }
      }
    }

    def computerTurn(): Unit = {
      val ((x, y), newRand) = rand.randomMove(board, rand)
      val newBoard = play(board, Cells.Red, x, y)
      MyIO.displayBoard(newBoard)

      if (hasContiguousLine(newBoard, Cells.Red)) {
        print("PC Venceu")
      } else
        gameLoop(newBoard, turn + 1, newRand, oldBoard, oldTurn)
    }

    val isEven = (n: Int) => n % 2 == 0
    if (isEven(turn)) {
      playerTurn()
    }
    else {
      computerTurn()
    }
  }


  def start(rand: MyRandom) {
    menuLoop(rand)
  }
}