package scalatetris.environment

/** 
 * Objeto compañero para la clase Stone que proporciona un constructor simplificado.
 */
object Stone {
  /** 
   * Crea una nueva pieza con un solo punto.
   * 
   * @param start Punto inicial de la pieza
   * @return Nueva pieza con un solo punto
   */
  def apply(start: Point): Stone = Stone(List(start))
}

/** 
 * Objeto que representa la pieza cuadrada del Tetris.
 * Esta pieza tiene forma de cuadrado 2x2 y no rota.
 */
object Square {
  /** 
   * Crea una nueva pieza cuadrada.
   * 
   * @param start Punto inicial desde donde se construye el cuadrado
   * @return Nueva pieza cuadrada
   */
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveRight(),
      start.moveDown(),
      start.moveDown().moveRight()
    ), "Square")
}

/** 
 * Objeto que representa la pieza larga (I) del Tetris.
 * Esta pieza tiene forma de línea vertical de 4 bloques.
 */
object Line {
  /** 
   * Crea una nueva pieza en forma de línea.
   * 
   * @param start Punto inicial desde donde se construye la línea
   * @return Nueva pieza en forma de línea
   */
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveDown(),
      start.moveDown().moveDown(),
      start.moveDown().moveDown().moveDown()
    ), "Line")
}

/** 
 * Objeto que representa la pieza L del Tetris.
 * Esta pieza tiene forma de L hacia la derecha.
 */
object LetterLLeft {
  /** 
   * Crea una nueva pieza en forma de L.
   * 
   * @param start Punto inicial desde donde se construye la L
   * @return Nueva pieza en forma de L
   */
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveDown(),
      start.moveRight(),
      start.moveRight().moveRight()
    ), "L")
}

/** 
 * Objeto que representa la pieza J del Tetris.
 * Esta pieza tiene forma de L invertida (hacia la izquierda).
 */
object LetterLRight {
  /** 
   * Crea una nueva pieza en forma de J.
   * 
   * @param start Punto inicial desde donde se construye la J
   * @return Nueva pieza en forma de J
   */
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveDown(),
      start.moveLeft(),
      start.moveLeft().moveLeft()
    ), "J")
}

/** 
 * Objeto que representa la pieza T del Tetris.
 * Esta pieza tiene forma de T invertida.
 */
object WinnerPodium {
  /** 
   * Crea una nueva pieza en forma de T.
   * 
   * @param start Punto inicial desde donde se construye la T
   * @return Nueva pieza en forma de T
   */
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveDown(),
      start.moveLeft(),
      start.moveRight()
    ), "T")
}

/** 
 * Objeto que representa la pieza S del Tetris.
 * Esta pieza tiene forma de S hacia la izquierda.
 */
object StepLeft {
  /** 
   * Crea una nueva pieza en forma de S.
   * 
   * @param start Punto inicial desde donde se construye la S
   * @return Nueva pieza en forma de S
   */
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveLeft(),
      start.moveLeft().moveDown(),
      start.moveLeft().moveDown().moveLeft()
    ), "S")
}

/** 
 * Objeto que representa la pieza Z del Tetris.
 * Esta pieza tiene forma de Z hacia la derecha.
 */
object StepRight {
  /** 
   * Crea una nueva pieza en forma de Z.
   * 
   * @param start Punto inicial desde donde se construye la Z
   * @return Nueva pieza en forma de Z
   */
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveRight(),
      start.moveRight().moveDown(),
      start.moveRight().moveDown().moveRight()
    ), "Z")
}

/** 
 * Clase que representa una pieza del Tetris.
 * 
 * Una pieza está compuesta por una lista de puntos que definen su forma
 * y un tipo que identifica qué clase de pieza es (T, L, J, etc.).
 * 
 * @param points Lista de puntos que conforman la pieza
 * @param stoneType Tipo de la pieza (Square, Line, T, L, J, S, Z, Default)
 */
case class Stone(points: List[Point], stoneType: String = "Default") {

  /** 
   * Mueve la pieza un espacio hacia abajo.
   * 
   * @return Nueva pieza con la posición actualizada
   */
  def moveDown(): Stone = copy(points = points.map(_.moveDown()))

  /** 
   * Mueve la pieza un espacio hacia arriba.
   * 
   * @return Nueva pieza con la posición actualizada
   */
  def moveUp(): Stone = copy(points = points.map(_.moveUp()))

  /** 
   * Mueve la pieza un espacio hacia la izquierda.
   * 
   * @return Nueva pieza con la posición actualizada
   */
  def moveLeft(): Stone = copy(points = points.map(_.moveLeft()))

  /** 
   * Mueve la pieza un espacio hacia la derecha.
   * 
   * @return Nueva pieza con la posición actualizada
   */
  def moveRight(): Stone = copy(points = points.map(_.moveRight()))

  /** 
   * Rota la pieza en sentido antihorario alrededor de su centro.
   * 
   * @return Nueva pieza con la rotación aplicada
   */
  def rotateLeft(): Stone = copy(points = points.map(_.rotateAroundCenterLeft(findRotationCenter)))

  /** 
   * Rota la pieza en sentido horario alrededor de su centro.
   * 
   * @return Nueva pieza con la rotación aplicada
   */
  def rotateRight(): Stone = copy(points = points.map(_.rotateAroundCenterRight(findRotationCenter)))
  
  /** 
   * Verifica si la pieza está dentro del marco del tablero.
   * 
   * @param size Tamaño del tablero
   * @return true si la pieza está completamente dentro del tablero
   */
  def isInFrame(size: Size): Boolean = points.forall(p => 
    p.x >= 0 && p.x < size.width && p.y >= 0 && p.y < size.height
  )

  /** 
   * Verifica si esta pieza colisiona con otra.
   * 
   * @param other Otra pieza para verificar colisión
   * @return true si hay al menos un punto en común entre las piezas
   */
  def doesCollide(other: Stone): Boolean = 
    points.exists(p => other.points.contains(p))

  /** 
   * Resetea la posición de la pieza a su posición inicial.
   * 
   * @return Nueva pieza en su posición inicial
   */
  def resetPosition(): Stone = {
    val minX = points.map(_.x).min
    val minY = points.map(_.y).min
    copy(points = points.map(p => Point(p.x - minX, p.y - minY)))
  }

  /** 
   * Mueve la pieza al centro superior del tablero.
   * 
   * @param center Punto central superior del tablero
   * @return Nueva pieza centrada en la parte superior
   */
  def toTopCenter(center: Point): Stone = {
    val resetStone = resetPosition()
    val width = resetStone.points.map(_.x).max - resetStone.points.map(_.x).min + 1
    val offsetX = center.x - width / 2
    val offsetY = center.y
    copy(points = resetStone.points.map(p => Point(p.x + offsetX, p.y + offsetY)))
  }

  /** 
   * Verifica si algún punto de la pieza está en la fila superior.
   * 
   * @return true si la pieza toca la fila superior, false en caso contrario
   */
  def isOnTop: Boolean = points.exists(_.isOnTop)

  private def findRotationCenter: Point = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    Point((xs.min + xs.max) / 2, (ys.min + ys.max) / 2)
  }
}
