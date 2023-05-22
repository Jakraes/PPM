import Cells.Board

import scala.annotation.tailrec

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

  def matrixToList[T](l: List[List[T]]): List[T] = l match {
    case Nil => Nil
    case x::xs => x ::: matrixToList(xs)
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
    case x => if (x.head == value) accX :: getIndexInList(x.tail, value, accX + 1) else getIndexInList(x.tail, value, accX + 1)
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
    case x :: xs => getIndexInList(x, value).map(y => (y, accY)) ::: getIndexInMatrix(xs, value, accY + 1)
  }

  /* Função que filtra uma lista de pares de indices para apenas aqueles que estarão no limite de uma dada matriz
   *
   * @param l   Matriz a ser avaliada
   * @param i   Lista de pares de indices
   * @return    Lista de pares de indices filtrada
   */
  def filterToBounds[T](l: List[List[T]], i: List[(Int, Int)]): List[(Int, Int)] = {
    i.filter { case (x, y) => x == 0 || x == l.size - 1 || y == 0 || y == l.size - 1 }
  }

  /* Função auxiliar que avalia se o jogador fez uma jogada válida
       *
       * @param board Tabuleiro do jogo
       * @param x     Coordenada X da jogada
       * @param y     Coordenada Y da jogada
       * @return      True se a jogada é válida, False caso contrário
       */
  def isValidMove(board: Board, x: Int, y: Int): Boolean = {
    //Range(0, size).contains(x) && Range(0, size).contains(y) && board(y)(x) == Cells.Empty
    val l_temp = 0 until board.size
    (l_temp contains x) && (l_temp contains y) && board(y)(x) == Cells.Empty
  }

  // T4
  def hasContiguousLine(board: Board, player: Cells.Cell) = {

    def isNeighborhood(cell: (Int, Int), center: (Int, Int)): Boolean = {
      val center_x = center._1
      val center_y = center._2

      val x = cell._1
      val y = cell._2

      //TODO double check e clean
      def aux(center_x: Int, center_y: Int, y: Int, x1: Int, x2: Int) = (center_y equals y) && (x1 :: x2 :: Nil contains center_x)

      aux(center_x, center_y, y, x - 1, x + 1) || aux(center_x, center_y, y - 1, x, x + 1) || aux(center_x, center_y, y + 1, x - 1, x)
    }

    val playerCells = Utils.getIndexInMatrix(board, player)

    //TODO

    // player é Blue
    val startNodes = playerCells filter ((c) => {
      if (Cells.Blue equals player) c._2 == 0 else c._1 == 0
    })

    def loop(hood: List[(Int, Int)], visited: List[(Int, Int)]): (Boolean, List[(Int, Int)]) = {
      hood match {
        case Nil => (false, visited)
        case h :: _ if (if (Cells.Blue equals player) h._2 equals board.size - 1 else h._1 equals board.size - 1) => (true, visited)
        case h :: t => {
          val (truthValue, visitedNodes) = loop(playerCells filter (isNeighborhood(_, h)) filterNot (h :: visited contains _), h :: visited)
          if (truthValue) (true, visitedNodes)
          else loop(t, h :: visited)
        }
      }
    }

    loop(startNodes, Nil)._1
  }


}
