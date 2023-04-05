case class MyIO() { //TODO object? Não pode por causa das coisas da pureza + erros no resto do código
  def getLine: String = scala.io.StdIn.readLine

  def getInt: Int = scala.io.StdIn.readInt

  def print(s: Any) = scala.Predef.print(s)

  def println(s: Any) = scala.Predef.println(s)
}
