import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.Label
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class MenuPrincipal {

  @FXML
  private var buttonContinue: Label = _

  @FXML
  private var buttonStart: Label = _

  @FXML
  private var buttonConfig: Label = _



  def buttonContinueClick(): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("GameScreen.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()


    buttonContinue.getScene.setRoot(mainViewRoot)
  }

  def buttonStartClick(): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("GameScreen.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()

    buttonStart.getScene.setRoot(mainViewRoot)

  }

  def buttonConfigClick(): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("MenuConfig.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()

    buttonConfig.getScene.setRoot(mainViewRoot)
  }
}
