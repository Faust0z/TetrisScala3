package scalatetris.environment

object Stone {
  def apply(start: Point): Stone = Stone(List(start))
}

object Square {
  def apply(start: Point): Stone =
    Stone(List(start.moveRight(), start.moveDown(), start.moveDown().moveRight()))
}

object Line {
  def apply(start: Point): Stone =
    Stone(List(start, start.moveDown(), start.moveDown().moveDown(), start.moveDown().moveDown().moveDown()))
}

object LetterLLeft {
  def apply(start: Point): Stone =
    Stone(List(start, start.moveDown(), start.moveRight(), start.moveRight().moveRight()))
}

object LetterLRight {
  def apply(start: Point): Stone =
    Stone(List(start, start.moveDown(), start.moveLeft(), start.moveLeft().moveLeft()))
}

object WinnerPodium {
  def apply(start: Point): Stone =
    Stone(List(start, start.moveDown(), start.moveLeft(), start.moveRight()))
}

object StepLeft {
  def apply(start: Point): Stone =
    Stone(List(start, start.moveLeft(), start.moveLeft().moveDown(), start.moveLeft().moveDown().moveLeft()))
}

object StepRight {
  def apply(start: Point): Stone =
    Stone(List(start, start.moveRight(), start.moveRight().moveDown(), start.moveRight().moveDown().moveRight()))
}

case class Stone(points: List[Point]) {

  def moveDown(): Stone = Stone(points.map(_.moveDown()))

  def moveLeft(): Stone = Stone(points.map(_.moveLeft()))

  def moveRight(): Stone = Stone(points.map(_.moveRight()))

  def rotateLeft(): Stone =
    if (points.isEmpty) this
    else Stone(points.map(_.rotateAroundCenterLeft(findRotationCenter())))

  def rotateRight(): Stone =
    if (points.isEmpty) this
    else Stone(points.map(_.rotateAroundCenterRight(findRotationCenter())))

  private def findRotationCenter(): Point = {
    val (min, max) = points.foldLeft((points.head, points.head)) {
      case ((min, max), point) => (min.min(point), max.max(point))
    }
    (max + min + Point(0, 1)) / 2
  }

  def toTopCenter(center: Point): Stone =
    if (points.isEmpty) this
    else {
      val min = points.reduceLeft(_.min(_))
      val stoneCenter = findRotationCenter()
      val xDiff = stoneCenter.x - center.x
      Stone(points.map(_ - Point(xDiff, min.y)))
    }

  def doesCollide(other: Stone): Boolean = points.exists(a => other.points.contains(a))

  def isInFrame(frame: Size): Boolean = points.forall(_.isInFrame(frame))

  def isOnTop: Boolean = points.exists(_.isOnTop)
}
