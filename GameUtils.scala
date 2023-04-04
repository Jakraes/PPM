object GameUtils {
  import scala.io.StdIn.readInt

  object GameUtils {
    def listAsString[T](lt: List[T]): String = lt match {
      case Nil => ""
      case x::xs => x.toString + " " + listAsString(xs)
    }

    def listOfElements[T](size: Int, ele: T): List[T] = size match {
      case 0 => Nil
      case _ => ele::listOfElements(size - 1, ele)
    }

    def changeMatrixValue[T](mt: List[List[T]], x: Int, y: Int, value: T): List[List[T]] =  mt updated(y, mt(y) updated(x, value))

    def getInput: Int  = readInt()
  }
}
