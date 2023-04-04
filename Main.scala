object Main extends App {

  //val game = new Game(7)
  //game.start()

  def getIndexInList[T](l: List[T], value: T, accX: Int = 0): List[Int] = l match {
    case Nil => Nil
    case x::xs => if (x == value) accX::getIndexInList(xs, value, accX + 1) else getIndexInList(xs, value, accX + 1)
  }

  def getIndexInMatrix[T](l: List[List[T]], value: T, accY: Int = 0): List[(Int, Int)] = l match {
    case Nil => Nil
    case x::xs => getIndexInList(x, value).map(y => (y, accY)):::getIndexInMatrix(xs, value, accY + 1)
  }

  val l = List(
    List(3, 2, 1),
    List(5, 3, 3),
    List(3, 1, 2)
  )
  println(getIndexInMatrix(l, 3)
}
