object Cells extends Enumeration {
  type Cell = Value
  type Board = List[List[Cells.Cell]]
  val Red = Value(s"${Console.RED}X")
  val Blue = Value(s"${Console.BLUE}0")
  val Empty = Value(s"${Console.RESET}.")

  // Obtido a partir de:
  // https://docs.scala-lang.org/overviews/core/string-interpolation.html
  // e
  // https://stackoverflow.com/questions/3664614/how-to-use-colours-in-console-output-in-scala-or-java
}