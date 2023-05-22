import Cells.Board
import Utils.createList

case class Game(rand: MyRandom, size: Int) {

  val board = Game.createBoard(size)

  def isValidMove(board: Board, x: Int, y: Int) = Utils.isValidMove(board, x, y)


  def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = Game.play(board, player, row, col)

  //def displayBoard(board: Board) = input.displayBoard(board) //TODO corrigir

  //TODO corrigir
  //def hasContiguousLine(board: Board, player: Cells.Cell) = Utils.hasContiguousLine(board, player)

 def start = Game.start(rand,size)
}

object Game{
   def createBoard(size: Int): Board = createList(size, createList(size, Cells.Empty))
  /* Função auxiliar que avalia se o jogador fez uma jogada válida
     *
     * @param board Tabuleiro do jogo
     * @param x     Coordenada X da jogada
     * @param y     Coordenada Y da jogada
     * @return      True se a jogada é válida, False caso contrário
     */

  // T2
   def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = {
    board updated(col, board(col) updated(row, player))
  }

   def start(rand: MyRandom,size: Int): Unit = {
    //MyIO.displayBoard(newBoard)
   // gameLoop(newBoard, rand = rand, oldBoard = newBoard, oldTurn = 0)
  }

}