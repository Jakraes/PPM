object Main extends App {
  val l = List(
    List(Cells.Empty, Cells.Empty, Cells.Empty),
    List(Cells.Empty, Cells.Empty, Cells.Empty),
    List(Cells.Empty, Cells.Empty, Cells.Empty)
  )
  val game = new Game(3)
  game.displayBoard(l)
}
