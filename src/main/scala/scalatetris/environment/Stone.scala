package scalatetris.environment

/**
 * Define la clase para las las piedras/piezas. Todas estas son una lista de Puntos que tienen un nombre que las identifica,
 * funcionalidades para moverlas, rotarlas y comprobar su posición en el board.
 *
 * @param points    Un parámetro de tipo Lista que guarda los puntos que componen a la pieza
 * @param stoneType Un parámetro de tipo String que indican qué tipo de pieza es
 */
case class Stone(points: List[Point], stoneType: String = "Default") {
  /**
   * Mueve todos los puntos de la pieza hacia abajo y la devuelve.
   */
  def moveDown(): Stone = copy(points = points.map(_.moveDown()))

  /**
   * Mueve todos los puntos de la pieza hacia la izquierda y la devuelve.
   */
  def moveLeft(): Stone = copy(points = points.map(_.moveLeft()))

  /**
   * Mueve todos los puntos de la pieza hacia la derecha y la devuelve.
   */
  def moveRight(): Stone = copy(points = points.map(_.moveRight()))

  /**
   * Rota todos los puntos de la pieza hacia la izquierda y la devuelve.
   */
  def rotateLeft(): Stone = copy(points = points.map(_.rotateAroundCenterLeft(findRotationCenter)))

  /**
   * Rota todos los puntos de la pieza hacia la derecha y la devuelve.
   */
  def rotateRight(): Stone = copy(points = points.map(_.rotateAroundCenterRight(findRotationCenter)))

  /**
   * Devuelve el punto de rotación según la pieza que esté en juego.
   */
  private def findRotationCenter: Point = stoneType match {
    //  Se podría incluir el centro de rotación directamente en la declaración de las Stones
    case "T" => points(1)
    case "L" => points(2)
    case "J" => points(2)
    case "Line" => points(1)
    case "S" => points(2)
    case "Z" => points(2)
    case _ => points.head
  }

  /**
   * Mueve la pieza en juego al centro y arriba del Board. Por defecto, las piezas comienzan en la esquina superior izquieda.
   */
  def toTopCenter(center: Point): Stone =
    if (points.isEmpty) this
    else {
      val min = points.reduceLeft(_.min(_))
      val stoneCenter = findRotationCenter
      val xDiff = stoneCenter.x - center.x
      copy(points = points.map(_ - Point(xDiff, min.y)))
    }

  /**
   * Mueve la pieza en juego al centro y arriba del Board luego de mantener una ficha y soltarla.
   */
  def resetPosition(): Stone = {
    val minX = points.map(_.x).min
    val minY = points.map(_.y).min
    copy(points = points.map(p => Point(p.x - minX, p.y - minY)))
  }

  /**
   * Devuelve True si algún punto de la pieza contacta con otros puntos, lo que significa que colisionó.
   */
  def doesCollide(other: Stone): Boolean = points.exists(a => other.points.contains(a))

  /**
   * Devuelve True si todos los puntos de la pieza están dentro del board.
   */
  def isInFrame(frame: Size): Boolean = points.forall(_.isInFrame(frame))

  /**
   * Devuelve True si ninguno de los puntos de la pieza sobresalen por encima del tablero. Usado para comprobar si
   * se terminó el juego.
   */
  def isOnTop: Boolean = points.exists(_.isOnTop)
}

object Square {
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveRight(),
      start.moveDown(),
      start.moveDown().moveRight()
    ), "Square")
}

object Line {
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveDown(),
      start.moveDown().moveDown(),
      start.moveDown().moveDown().moveDown()
    ), "Line")
}

object LetterLLeft {
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveDown(),
      start.moveRight(),
      start.moveRight().moveRight()
    ), "L")
}

object LetterLRight {
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveDown(),
      start.moveLeft(),
      start.moveLeft().moveLeft()
    ), "J")
}

object WinnerPodium {
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveDown(),
      start.moveLeft(),
      start.moveRight()
    ), "T")
}

object StepLeft {
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveLeft(),
      start.moveLeft().moveDown(),
      start.moveLeft().moveDown().moveLeft()
    ), "S")
}

object StepRight {
  def apply(start: Point): Stone =
    Stone(List(
      start,
      start.moveRight(),
      start.moveRight().moveDown(),
      start.moveRight().moveDown().moveRight()
    ), "Z")
}
