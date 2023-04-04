object Utils {
  def createList[T](size: Int, ele: T): List[T] = size match {
    case 0 => Nil
    case _ => ele::createList(size - 1, ele)
  }
}
