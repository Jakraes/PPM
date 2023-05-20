import Cells.Board
import Game.hasContiguousLine
import MyIO.displayBoard

object teste extends App{

  def get():Board = {
    List(Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty ) ::
      List(Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty ) ::
      List(Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty , Cells.Red ) ::
      List(Cells.Red , Cells.Red , Cells.Red , Cells.Red , Cells.Empty ) ::
      List(Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty ) :: Nil
  }
  val board :Board = get()

  displayBoard(board)
  MyIO.print(hasContiguousLine(board,Cells.Red))
}
