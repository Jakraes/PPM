import GameUtils.{getInput, listAsString}

import scala.annotation.tailrec
import scala.util.Random



class Game(val size: Int, playerOne: Char, playerTwo: Char) {
  val defaultGameState: List[List[Char]] = GameUtils.listOfElements(size, GameUtils.listOfElements(size, '.'))

  Random.setSeed(0)

  /* Função que desenha o tabuleiro de jogo
   *
   * @param mt  O tabuleiro do jogo a ser desenhado
   */
  private def drawBoard(mt: List[List[Char]]): Any = {
    @tailrec
    def drawMiddleBoard(mt: List[List[Char]], acc: Int = 0): Unit = {
      mt match {
        case Nil => Nil
        case x :: xs => {
          println(" " * acc + playerTwo + " " + listAsString(x) + playerTwo)
          drawMiddleBoard(xs, (acc + 1))
        }
      }
    }

    println((" " + playerOne) * size)
    drawMiddleBoard(mt)
    println(" " * size + (" " + playerOne) * size)
  }

  private def placePiece(mt: List[List[Char]], x: Int, y: Int, piece: Char = '0') = GameUtils.changeMatrixValue(mt, x, y, piece)

  /* Função que coloca uma peça aleatória no tabuleiro
   *
   * @param mt  Tabuleiro do jogo, aka gameState
   * @return    Tabuleiro de jogo com mais uma peça inserida
   */
  @tailrec
  private def placeRandomPiece(mt: List[List[Char]]): List[List[Char]] = {
    val x = Random.nextInt(size)
    val y = Random.nextInt(size)
    mt(y)(x) match {
      case '.' => placePiece(mt, x, y, 'X')
      case _ => placeRandomPiece(mt)
    }
  }

  private def answerUserInput(mt: List[List[Char]]): List[List[Char]] = {
    print("X: ")
    val x = getInput
    print("Y: ")
    val y = getInput
    mt(y)(x) match {
      case '.' => placePiece(mt, x, y)
      case _ => println("Already occupied!"); answerUserInput(mt)
    }
  }

  def gameLoop(gameState: List[List[Char]] = defaultGameState, acc: Int = 0): Unit = {
    println("<" + "-" * size + ">")
    drawBoard(gameState)
    if (acc % 2 == 0) {
      println("CPU Turn")
      gameLoop(placeRandomPiece(gameState), acc + 1)
    }
    else {
      println("Your Turn")
      gameLoop(answerUserInput(gameState), acc + 1)
    }
  }
}
