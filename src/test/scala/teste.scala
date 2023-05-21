import Cells.Board
import MyIO.displayBoard
import Utils.hasContiguousLine

object teste extends App{

  def get():Board = {
    List(Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty ) ::
      List(Cells.Empty , Cells.Empty , Cells.Red , Cells.Red , Cells.Empty ) ::
      List(Cells.Empty , Cells.Red , Cells.Empty , Cells.Red , Cells.Empty ) ::
      List(Cells.Red , Cells.Red , Cells.Red , Cells.Red , Cells.Red ) ::
      List(Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty , Cells.Empty ) :: Nil
  }
  val board :Board = get()

  displayBoard(board)
  MyIO.print(hasContiguousLine(board,Cells.Red))
}
