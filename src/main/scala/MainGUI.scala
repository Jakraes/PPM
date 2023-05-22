import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class MainGUI extends Application {
  // Mudei a criação do MyRandom e do MyInput para a main para ser mais puro, o que acham? - João C

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Hex")
    val fxmlLoader = new FXMLLoader(getClass.getResource("MenuPrincipal.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

object FxApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[MainGUI], args: _*)
  }
}
