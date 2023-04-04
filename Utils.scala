object Utils {
  def createList[T](size: Int, ele: T): List[T] = size match {
    case 0 => Nil
    case _ => ele::createList(size - 1, ele)
  }

  def getIndexInList[T](l: List[T], value: T, accX: Int = 0): List[Int] = l match {
    case Nil => Nil
    case x::xs => if (x == value) accX::getIndexInList(xs, value, accX + 1) else getIndexInList(xs, value, accX + 1)
  }

  def getIndexInMatrix[T](l: List[List[T]], value: T, accY: Int = 0): List[(Int, Int)] = l match {
    case Nil => Nil
    case x::xs => getIndexInList(x, value).map(y => (y, accY)):::getIndexInMatrix(xs, value, accY + 1)
  }
}
