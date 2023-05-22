import Game.play
import Main.gameLoop
import Utils.{hasContiguousLine, isValidMove}
import javafx.fxml.FXML
import javafx.scene.paint.Color
import javafx.scene.shape.Polygon

class GameScreen {


  val game = Game(MyRandom(0),5)
  game.start()

  var board = game.board
  var rand = game.rand
  @FXML
  private var hex00: Polygon = _

  @FXML
  private var hex01: Polygon = _

  @FXML
  private var hex02: Polygon = _

  @FXML
  private var hex03: Polygon = _

  @FXML
  private var hex04: Polygon = _

  @FXML
  private var hex10: Polygon = _

  @FXML
  private var hex11: Polygon = _

  @FXML
  private var hex12: Polygon = _

  @FXML
  private var hex13: Polygon = _

  @FXML
  private var hex14: Polygon = _


  @FXML
  private var hex20: Polygon = _

  @FXML
  private var hex21: Polygon = _

  @FXML
  private var hex22: Polygon = _

  @FXML
  private var hex23: Polygon = _

  @FXML
  private var hex24: Polygon = _


  @FXML
  private var hex30: Polygon = _

  @FXML
  private var hex31: Polygon = _

  @FXML
  private var hex32: Polygon = _

  @FXML
  private var hex33: Polygon = _

  @FXML
  private var hex34: Polygon = _


  @FXML
  private var hex40: Polygon = _

  @FXML
  private var hex41: Polygon = _

  @FXML
  private var hex42: Polygon = _

  @FXML
  private var hex43: Polygon = _


  @FXML
  private var hex44: Polygon = _


  def hex00Clicked(): Unit = {
    if (isValidMove(board, 0, 0)) {
      hex00.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 0, 0)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex01Clicked(): Unit = {
    if (isValidMove(board, 0, 1)) {
      hex01.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 0, 1)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
    }


  private def computerTurn(): Unit = {
    val ((x, y), randTemp) = game.rand.randomMove(board, rand)
    rand = randTemp
    board = play(board, Cells.Red, x, y)
    if (hasContiguousLine(board, Cells.Red)) {
      print("PC Venceu")
    }

    x match {
      case 0 =>{y match {
        case 0 => hex00.setFill(Color.RED)
        case 1 => hex01.setFill(Color.RED)
        case 2 => hex02.setFill(Color.RED)
        case 3 => hex03.setFill(Color.RED)
        case 4 => hex04.setFill(Color.RED)
        case _ =>{}

      }}
      case 1 => {
        y match {
          case 0 => hex10.setFill(Color.RED)
          case 1 => hex11.setFill(Color.RED)
          case 2 => hex12.setFill(Color.RED)
          case 3 => hex13.setFill(Color.RED)
          case 4 => hex14.setFill(Color.RED)
          case _ => {}

        }
      }
      case 2 => {
        y match {
          case 0 => hex20.setFill(Color.RED)
          case 1 => hex21.setFill(Color.RED)
          case 2 => hex22.setFill(Color.RED)
          case 3 => hex23.setFill(Color.RED)
          case 4 => hex24.setFill(Color.RED)

          case _ => {}

        }
      }
      case 3 => {
        y match {
          case 0 => hex30.setFill(Color.RED)
          case 1 => hex31.setFill(Color.RED)
          case 2 => hex32.setFill(Color.RED)
          case 3 => hex33.setFill(Color.RED)
          case 4 => hex34.setFill(Color.RED)

          case _ => {}
        }
      }

      case 4 => {
        y match {
          case 0 => hex40.setFill(Color.RED)
          case 1 => hex41.setFill(Color.RED)
          case 2 => hex42.setFill(Color.RED)
          case 3 => hex43.setFill(Color.RED)
          case 4 => hex44.setFill(Color.RED)

          case _ => {}
        }
      }
      case _ =>{}
    }
  }

  def hex02Clicked(): Unit = {
    if (isValidMove(board, 0, 2)) {
      hex02.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 0, 2)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex03Clicked(): Unit = {
    if (isValidMove(board, 0, 3)) {
      hex03.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 0, 3)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex04Clicked(): Unit = {
    if (isValidMove(board, 0, 4)) {
      hex04.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 0, 4)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }


  def hex10Clicked(): Unit = {

    if (isValidMove(board, 1, 0)) {
      hex10.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 1, 0)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex11Clicked(): Unit = {
    if (isValidMove(board,1, 1)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 1, 1)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex12Clicked(): Unit = {
    if (isValidMove(board, 1, 2)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 1, 2)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex13Clicked(): Unit = {
    if (isValidMove(board, 1, 3)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 1, 3)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex14Clicked(): Unit = {
    if (isValidMove(board, 1, 4)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 1, 4)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }


  def hex20Clicked: Unit = {
    if (isValidMove(board, 2, 0)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 2, 0)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex21Clicked(): Unit = {
    if (isValidMove(board, 2, 1)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 2, 1)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex22Clicked(): Unit = {
    if (isValidMove(board, 2, 2)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 2, 2)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex23Clicked(): Unit = {
    if (isValidMove(board, 2, 3)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 2, 3)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex24Clicked(): Unit = {
    if (isValidMove(board, 2, 4)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 2, 4)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }


  def hex30Clicked(): Unit = {
    if (isValidMove(board, 3, 0)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 3, 0)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex31Clicked(): Unit = {
    if (isValidMove(board, 3, 1)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 3, 1)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex32Clicked(): Unit = {
    if (isValidMove(board, 3, 2)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 3, 2)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex33Clicked(): Unit = {
    if (isValidMove(board, 3, 3)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 3, 3)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex34Clicked(): Unit = {
    if (isValidMove(board, 3, 4)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 3, 4)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }


  def hex40Clicked(): Unit = {
    if (isValidMove(board, 4, 0)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 4, 0)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex41Clicked(): Unit = {
    if (isValidMove(board, 4, 1)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 4, 1)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }

  def hex42Clicked: Unit = {
    if (isValidMove(board, 4,2)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 4, 2)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }  }

  def hex43Clicked: Unit = {
    if (isValidMove(board, 4, 3)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 4, 3)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }  }


  def hex44Clicked: Unit = {
    if (isValidMove(board, 4, 4)) {
      hex11.setFill(Color.DODGERBLUE)
      board = play(board, Cells.Blue, 4, 4)
      if (hasContiguousLine(board, Cells.Blue))
        MyIO println ("Tu venceste!! :)")

      computerTurn()
    }
  }
}