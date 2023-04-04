object Cells extends Enumeration {
  type Cell = Value
  // Obtido a partir de https://docs.scala-lang.org/overviews/core/string-interpolation.html
  val Red = Value(s"${Console.RED}X")
  val Blue = Value(s"${Console.BLUE}0")
  val Empty = Value(s"${Console.RESET}.")
}
