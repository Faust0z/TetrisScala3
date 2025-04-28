package scalatetris.environment

/**
 * Clase que representa un punto en el espacio bidimensional.
 *
 * Esta clase es inmutable y proporciona operaciones básicas para manipular
 * coordenadas en el juego Tetris, como movimientos y rotaciones.
 *
 * @param x Coordenada horizontal
 * @param y Coordenada vertical
 */
case class Point(x: Int, y: Int) {

  def moveDown(): Point = Point(x, y + 1)
  /**
   * Mueve el punto una unidad hacia la izquierda.
   *
   * @return Nuevo punto con x decrementado en 1
   */
  def moveLeft(): Point = Point(x - 1, y)
  /**
   * Mueve el punto una unidad hacia la derecha.
   *
   * @return Nuevo punto con x incrementado en 1
   */
  def moveRight(): Point = Point(x + 1, y)

  /**
   * Mueve el punto una unidad hacia la izquierda.
   *
   * @return Nuevo punto con x decrementado en 1
   */



 

  /**
   * Rota el punto en sentido antihorario alrededor de un centro.
   *
   * @param center Punto alrededor del cual rotar
   * @return Nuevo punto rotado 90 grados en sentido antihorario
   */
  def rotateAroundCenterLeft(center: Point): Point = {
    center + Point(-(this - center).y, (this - center).x)
  }

  def rotateAroundCenterRight(center: Point): Point = {
    center + Point((this - center).y, -(this - center).x)
  }

  /**
   * Compara dos puntos para encontrar el mínimo según sus coordenadas.
   *
   * @param other Punto a comparar
   * @return Punto con las coordenadas mínimas entre ambos puntos
   */
  def min(other: Point): Point = Point(math.min(x, other.x), math.min(y, other.y))

  /**
   * Verifica si el punto está dentro del marco especificado.
   *
   * @param frame Tamaño del marco a verificar
   * @return true si el punto está dentro del marco, false en caso contrario
   */
  def isInFrame(frame: Size): Boolean =
    x >= 0 && x < frame.width && y >= 0 && y < frame.height

  /**
   * Verifica si el punto está en la fila superior (y = 0).
   *
   * @return true si el punto está en la fila superior, false en caso contrario
   */
  def isOnTop: Boolean = y == 0

  def +(other: Point): Point = Point(x + other.x, y + other.y)

  def -(other: Point): Point = Point(x - other.x, y - other.y)
}