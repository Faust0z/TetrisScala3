package scalatetris.environment

/**
 * Representa un punto en el board del juego, indicado con las posiciones: x, y.
 * Es la clase básica para crear las piezas del juego: cada pieza está compuesta por un conjunto de puntos
 */
case class Point(x: Int, y: Int) {

  /**
   * Mueve un punto hacia abajo. Es usado para mover todos los puntos de una pieza hacia abajo
   */
  def moveDown(): Point = Point(x, y + 1)

  /**
   * Mueve un punto hacia la izquierda. Es usado para mover todos los puntos de una pieza hacia la izquierda
   */
  def moveLeft(): Point = Point(x - 1, y)

  /**
   * Mueve un punto hacia la derecha. Es usado para mover todos los puntos de una pieza hacia la derecha
   */
  def moveRight(): Point = Point(x + 1, y)

  /**
   * Rota un punto de la pieza hacia la izquierda.
   * @param center Un parámetro de tipo Point que indica cuál es el punto de rotación de la pieza (varía según piedra)
   */
  def rotateAroundCenterLeft(center: Point): Point = {
    center + Point(-(this - center).y, (this - center).x)
  }

  /**
   * Rota un punto de la pieza hacia la derecha.
   * @param center Un parámetro de tipo Point que indica cuál es el punto de rotación de la pieza (varía según piedra)
   */
  def rotateAroundCenterRight(center: Point): Point = {
    center + Point((this - center).y, -(this - center).x)
  }

  /**
   * Devuelve un nuevo punto con las coordenadas mínimas entre este punto y otro.
   * Se usa para encontrar el punto más arriba y más a la izquierda de una pieza para ubicarla en la board
   * al crearlas.
   * @param other Un parámetro de tipo Point para compararlo con otro  
   */
  def min(other: Point): Point = Point(math.min(x, other.x), math.min(y, other.y))

  /**
   * Comprueba si un punto está dentro del board. Es usado para saber si una pieza está dentro del board. 
   * @param frame Un parámetro de tipo Point que indica cuál es el punto de rotación de la pieza (varía según piedra)
   */
  def isInFrame(frame: Size): Boolean =
    x >= 0 && x < frame.width && y >= 0 && y < frame.height

  /**
   * Devuelve true si el punto está en la primera fila del board.
   * Usado para saber si alguna parte de una pieza está en la parte superior del board.
   */
  def isOnTop: Boolean = y == 0

  /**
   * Sobreescribimos la suma para facilitar la suma de los puntos de una pieza. Permite hacer operaciones "Point + Point"
   * @param other Un parámetro de tipo Point que indica cuánto se sumará
   */
  def +(other: Point): Point = Point(x + other.x, y + other.y)

  /**
   * Sobreescribimos la resta para facilitar la resta de los puntos de una pieza. Permite hacer operaciones "Point - Point"
   * @param other Un parámetro de tipo Point que indica cuánto se restará
   */
  def -(other: Point): Point = Point(x - other.x, y - other.y)
}
