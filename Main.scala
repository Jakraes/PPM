object Main {
  def main(args: Array[String]) = {
    // Mudei a criação do MyRandom e do MyInput para a main para ser mais puro, o que acham? - João C
    val io = new MyIO
    val rand = new MyRandom(0) // Aqui temos que
    val game = Game(rand, io)
    game.start
  }
}
