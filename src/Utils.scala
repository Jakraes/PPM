object Utils {
  /* Função que cria uma lista de um certo tamanho preenchida com o mesmo elemento
   *
   * @param size  Tamanho da lista a ser criada
   * @param ele   Elemento que vai preencher a lista
   * @return      Lista de tamanho size preenchida com ele
   */
  def createList[T](size: Int, ele: T): List[T] = {
    @tailrec
    def loop(l: List[T], size: Int): List[T] = if (size equals 0) l else loop(ele :: l, size - 1)

    loop(Nil, size)
  }


  /* Função que retorna todos os indices onde foi encontrado um certo valor na lista
   *
   * @param l     Lista a ser avaliada
   * @param value Valor que se vai tentar encontrar
   * @param accX  Acumulador
   * @return      Lista com os indices onde foi encontrado value
   */
  def getIndexInList[T](l: List[T], value: T, accX: Int = 0): List[Int] = l match {
    case Nil => Nil
    case x::xs => if (x == value) accX::getIndexInList(xs, value, accX + 1) else getIndexInList(xs, value, accX + 1)
  }

  /* Função que retorna todos os pares de indices onde foi encontrado um certo valor na matriz
   *
   * @param l     Matriz a ser avaliada
   * @param value Valor que se vai tentar encontrar
   * @param accY  Acumulador
   * @return      Lista com os pares de indices onde foi encontrado value
   */
  def getIndexInMatrix[T](l: List[List[T]], value: T, accY: Int = 0): List[(Int, Int)] = l match {
    case Nil => Nil
    case x::xs => getIndexInList(x, value).map(y => (y, accY)):::getIndexInMatrix(xs, value, accY + 1)
  }

  /* Função que filtra uma lista de pares de indices para apenas aqueles que estarão no limite de uma dada matriz
   *
   * @param l   Matriz a ser avaliada
   * @param i   Lista de pares de indices
   * @return    Lista de pares de indices filtrada
   */
  def filterToBounds[T](l: List[List[T]], i: List[(Int, Int)]): List[(Int, Int)] = {
    i.filter{case (x, y) => x == 0 || x == l.size - 1 || y == 0 || y == l.size - 1}
  }

  // T4
  def checkConnection[T](l: List[List[T]], value: T): Boolean = {
    val indexes = filterToBounds(l, getIndexInMatrix(l, value))

    def auxSearch(l: List[List[T]], i: List[(Int, Int)]) = i match {
      case Nil => Nil
      case x::xs => {

      }
    }

    true // Placeholder, tem que ser mudado depois
  }
}
