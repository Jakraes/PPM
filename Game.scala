import Cells.Board
import Utils.createList

import scala.::
import scala.annotation.tailrec

case class Game(rand: MyRandom) {
  val input = MyIO
  def isValidMove(board: Board, x: Int, y: Int) = Game.isValidMove(board, x, y)

  //TODO corrigir?
  def randomMove(board: Board) = rand.randomMove(board,rand)

  def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = Game.play(board, player, row, col)

  def displayBoard(board: Board) = input.displayBoard(board) //TODO corrigir?

  def hasContiguousLine(board: Board, player: Cells.Cell) = Game.hasContiguousLine(board, player)

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
  private def isValidMove(board: Board, x: Int, y: Int): Boolean = {
    //Range(0, size).contains(x) && Range(0, size).contains(y) && board(y)(x) == Cells.Empty
    val l_temp = 0 until board.size
    (l_temp contains x) && (l_temp contains y) && board(y)(x) == Cells.Empty
  }

  // T2
  private def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = {
    board updated(col, board(col) updated(row, player))
  }


  // TODO T4
  private def hasContiguousLine(board: Board, player: Cells.Cell) = {

  def getNeighborhood(cell:(Int,Int),center:(Int,Int)):Boolean = {
    val center_x = center._1
    val center_y = center._2

    val x = cell._1
    val y = cell._2

    //TODO double check e clean
    def aux(center_x:Int,center_y:Int,y:Int, x1:Int,x2:Int) = (center_y equals  y) && (x1 :: x2 :: Nil  contains center_x )
    aux(center_x,center_y,y ,x-1,x+1)   || aux(center_x,center_y,y - 1 ,x,x+1) || aux(center_x,center_y,y + 1 ,x-1,x)
  }
    val playerCells = Utils.getIndexInMatrix(board, player)


    //TODO finalizar
    
    // player é Blue
    val startNodes = playerCells filter ((c)=> {c._2 == 0})

    def loop(hood:List[(Int,Int)],prev:(Int,Int)):Boolean ={
      hood match {
        case Nil => false
        case h :: t if(h._2 == board.size - 1) => true
        case h :: t =>loop(playerCells filter( getNeighborhood(_,h) ) filterNot (_ == prev), h) ||
          loop(t,prev)
        }
      }
    loop(startNodes,(-20,-20))
    }


  // TODO T6
  // Arranjar maneira desta função funcionar sem dar return a Any
  private def menuLoop(rand: MyRandom, size: Int = 5, diff: Int = 0): Any = {
    val input = MyIO
    MyIO.print(s"${Console.RESET}Menu principal:\n")

    input.print(s"${Console.RESET}\t1: Começar jogo \n\t2: Configurar jogo \n\tQualquer tecla: Sair\n> ")
    val i = input.getLine

    i match {
      case "1" => {
        val newBoard = createBoard(size)
        input.displayBoard(newBoard)
        gameLoop(newBoard, rand = rand, oldBoard = newBoard, oldTurn = 0)
      }
      case "2" => {
        menuConfig(rand,  size, diff)
      }
      case _ => {}
    }
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
            val oldBoard = board
            val newBoard = play(board, Cells.Blue, x, y)
            MyIO.displayBoard(newBoard)
            if (hasContiguousLine(newBoard, Cells.Blue))
              MyIO.println("Tu vences-te!! :)")
            else
              gameLoop(newBoard, turn + 1, rand, oldBoard, turn) // T5
          }
          else {
            MyIO.println("Invalid move")
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