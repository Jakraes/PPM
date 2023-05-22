import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Node, Parent, Scene}
import javafx.scene.control.Label
import javafx.stage.Stage

class MenuPrincipal {

  @FXML
  private var buttonContinue: Label = _

  @FXML
  private var buttonStart: Label = _

  @FXML
  private var buttonConfig: Label = _

  def buttonContinueClick(): Unit = {

  }

  def buttonStartClick(): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("FX/GameScreen.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)

    val stage = new Stage()
    stage.setScene(scene)
    stage.show()
  }

  def buttonConfigClick(): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("FX/MenuConfig.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)

    val stage = new Stage()
    stage.setScene(scene)
    stage.show()
  }
}
