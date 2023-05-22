import Cells.Board
import Game.play
import Utils.{hasContiguousLine, isValidMove}

// TODO T6
object Main extends App {

  //TODO
  def menuConfig(rand: MyRandom, size: Int = 0, diff: Int = 0, humanFirst: Boolean = true): Any = {
    MyIO.print(s"${Console.RESET}Menu de configurações:\n")
    MyIO.print(s"${Console.RESET}\tClique em:\n\t\t1: Mudar tamanho do tabuleiro \n\t\t2: Mudar seed \n\t\t3: Escolher ser primeiro ou não \n\t\tQualquer tecla: Voltar \n> ")
    val j = MyIO.getLine
    j match {
      case "1" => {
        MyIO.print(s"${Console.RESET}Tamanho: ")
        val newSize = MyIO.getInt
        menuConfig(rand, newSize, diff, humanFirst)
      }
      case "2" => {
        MyIO.print(s"${Console.RESET}Seed: ")
        val newSeed = MyIO.getInt
        menuConfig(new MyRandom(newSeed), size, -1, humanFirst)
      }
      case "3" => {
        MyIO.print(s"${Console.RESET}1 - Para seres primeiro\t2 - Para seres em segundo: ")
        val newHumanFirst = MyIO.getInt equals 1
        menuConfig(rand, size, diff, newHumanFirst)
      }
      case _ => menuLoop(rand, size, diff, humanFirst)
    }
  }

  // TODO: the one bellow might need cleaning
  def gameLoop(board: Board, turn: Int = 0, rand: MyRandom, oldBoard: Board, oldTurn: Int) {
    def playerTurn(): Unit = {
      // Turno do jogador
      MyIO.print(s"${Console.RESET}\tClique em:\n\t1. Fazer jogada. \n\t2. Undo. \n\t3. Guardar Jogo. \n\tQualquer tecla. Abandonar jogo. \n> ")
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
              MyIO println ("Tu venceste!! :)")
            else
              gameLoop(newBoard, turn + 1, rand, oldBoard, turn) // T5
          }
          else {
            MyIO println ("Jogada invalida")
            gameLoop(board, turn, rand, oldBoard, oldTurn) // T5
          }
        }
        case "2" => {
          MyIO displayBoard (oldBoard)
          gameLoop(oldBoard, oldTurn, rand, oldBoard, oldTurn) // T5
        }
        case "3" => {
          MyIO.saveGame(turn, board, rand)
          gameLoop(board, turn, rand, oldBoard, oldTurn)
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

  // TODO Arranjar maneira desta função funcionar sem dar return a Any
  private def menuLoop(rand: MyRandom, size: Int = 5, diff: Int = 0, humanFirst: Boolean = true) = {
    val input = MyIO
    MyIO.print(s"${Console.RESET}Menu principal:\n")

    input.print(s"${Console.RESET}\t1: Novo jogo \n\t2: Continuar Jogo \n\t3: Configurar jogo \n\tQualquer tecla: Sair\n> ")
    val i = input.getLine

    i match {
      case "1" => {
        val game = Game(rand, size)
        game.start()

        MyIO.displayBoard(game.board)
        if (humanFirst) gameLoop(game.board, rand = rand, oldBoard = game.board, oldTurn = 0)
        else gameLoop(game.board, turn = 1, rand = rand, oldBoard = game.board, oldTurn = 0)
      }
      case "2" => {
        //TODO NAO FUNCIONA

        /*val (turn, board, rand) = MyIO.loadGame()
        val game = (rand, board.size)

        MyIO.displayBoard(board)
        gameLoop(board, turn = turn, rand = rand, oldBoard = board, oldTurn = turn - 1)
        */
      }
      case "3" => {
        menuConfig(rand, size, diff, humanFirst)
      }
      case _ => {}
    }
  }

  menuLoop(new MyRandom(0))
}
