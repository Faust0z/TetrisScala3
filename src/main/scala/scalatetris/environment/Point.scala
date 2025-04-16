package scalatetris.environment

case class Point(x: Int, y: Int) {

  def moveDown(): Point = Point(x, y + 1)

  def moveLeft(): Point = Point(x - 1, y)

  def moveRight(): Point = Point(x + 1, y)

  def rotateAroundCenterLeft(center: Point): Point = {
    val rotated = (this - center).rotateLeft()
    center + rotated
  }

  def rotateAroundCenterRight(center: Point): Point = {
    val rotated = (this - center).rotateRight()
    center + rotated
  }

  private[environment] def rotateLeft(): Point = Point(-y, x)

  private[environment] def rotateRight(): Point = Point(y, -x)

  def max(other: Point): Point = Point(math.max(x, other.x), math.max(y, other.y))

  def min(other: Point): Point = Point(math.min(x, other.x), math.min(y, other.y))

  def isInFrame(frame: Size): Boolean =
    x >= 0 && x < frame.width && y >= 0 && y < frame.height

  def isOnTop: Boolean = y == 0

  def +(other: Point): Point = Point(x + other.x, y + other.y)

  def -(other: Point): Point = Point(x - other.x, y - other.y)

  def *(factor: Int): Point = Point(x * factor, y * factor)

  def /(divisor: Int): Point = Point(x / divisor, y / divisor)
}
