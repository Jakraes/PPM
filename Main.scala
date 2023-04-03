object Main {

  def main(args: Array[String]): Unit = {
    val l = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)
    )

    val l1 = GameUtils.listOfElements(3, ".")
    println(l1)
  }
}