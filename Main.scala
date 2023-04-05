object Main extends App {

  val game = new Game(7)
  game.start()
  val l = List(
    List(3, 2, 1),
    List(5, 3, 3),
    List(3, 1, 2)
  )
  println(filterToBounds(l, getIndexInMatrix(l, 3)))


  def getIndexInList[T](l: List[T], value: T, accX: Int = 0): List[Int] = l match {
    case Nil => Nil
    case x::xs => if (x == value) accX::getIndexInList(xs, value, accX + 1) else getIndexInList(xs, value, accX + 1)
  }

  def getIndexInMatrix[T](l: List[List[T]], value: T, accY: Int = 0): List[(Int, Int)] = l match {
    case Nil => Nil
    case x::xs => getIndexInList(x, value).map(y => (y, accY)):::getIndexInMatrix(xs, value, accY + 1)
  }

  def filterToBounds[T](l: List[List[T]], i: List[(Int, Int)]): List[(Int, Int)] = {
    i filter  (x, y) => x == 0 || x == l.size - 1 || y == 0 || y == l.size - 1
  }
}
