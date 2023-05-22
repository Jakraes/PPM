object Main extends App{
    // Mudei a criação do MyRandom e do MyInput para a main para ser mais puro, o que acham? - João C
   Game(new MyRandom(0), 5).start
}
