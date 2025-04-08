package scalatetris.environment

case class Point(val x: Int, val y: Int) {
  
  def moveDown() = Point(x, y+1)
  
  def moveLeft() = Point(x-1, y)
  
  def moveRight() = Point(x+1, y)
  
  def rotateAroundCenterLeft(center: Point) = {
    val diff = this - center
    val rotated = diff.rotateLeft
    center + rotated
  }
  
  def rotateAroundCenterRight(center: Point) = {
    val diff = this - center
    val rotated = diff.rotateRight
    center + rotated
  }
  
  private def rotateLeft() = rotate(math.Pi / 2)
  
  private def rotateRight() = rotate(-math.Pi / 2)
  
  private def rotate(angle: Double) = {
    val x = this.x * math.cos(angle) - this.y * math.sin(angle);
    val y = this.x * math.sin(angle) + this.y * math.cos(angle);
    Point(math.round(x).asInstanceOf[Int], math.round(y).asInstanceOf[Int])
  }
  
  def max(other: Point) = Point(math.max(x, other.x), math.max(y, other.y))
  
  def min(other: Point) = Point(math.min(x, other.x), math.min(y, other.y))
  
  def isInFrame(frame: Size) = (0 until frame.width).contains(x) &&
                                (0 until frame.height).contains(y)
                                
  def isOnTop(): Boolean = y == 0
  
  def +(other: Point) = Point(x + other.x, y + other.y)
  
  def -(other: Point) = Point(x - other.x, y - other.y)
  
  def *(factor: Int) = Point(x * factor, y * factor)
  
  def /(divisor: Int) = Point(x / divisor, y / divisor)
}
