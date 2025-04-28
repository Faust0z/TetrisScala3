package scalatetris.environment

case class Point(x: Int, y: Int) {

  def moveDown(): Point = Point(x, y + 1)

  def moveLeft(): Point = Point(x - 1, y)

  def moveRight(): Point = Point(x + 1, y)

  def rotateAroundCenterLeft(center: Point): Point = {
    center + Point(-(this - center).y, (this - center).x)
  }

  def rotateAroundCenterRight(center: Point): Point = {
    center + Point((this - center).y, -(this - center).x)
  }

  def min(other: Point): Point = Point(math.min(x, other.x), math.min(y, other.y))

  def isInFrame(frame: Size): Boolean =
    x >= 0 && x < frame.width && y >= 0 && y < frame.height

  def isOnTop: Boolean = y == 0

  def +(other: Point): Point = Point(x + other.x, y + other.y)

  def -(other: Point): Point = Point(x - other.x, y - other.y)
}
