import GameUtils.listAsString

class Game(val size: Int, playerOne: Char, playerTwo: Char) {


  def drawBoard(acc: Int = 0, m: List[List[Char]]): Unit = m match {
    case Nil => Nil
    case x::xs => println(" " * acc + listAsString(x)); drawBoard(acc + 1, xs)
  }

  def gameLoop(): Unit = {

    gameLoop()
  }
}
