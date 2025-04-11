package scalatetris.environment

object Stone {
  def apply(start: Point): Stone = Stone(List(start), "Default")
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

case class Stone(points: List[Point], stoneType: String = "Default") {

  def moveDown(): Stone = copy(points = points.map(_.moveDown()))

  def moveLeft(): Stone = copy(points = points.map(_.moveLeft()))

  def moveRight(): Stone = copy(points = points.map(_.moveRight()))

  def rotateLeft(): Stone =
    if (points.isEmpty) this
    else copy(points = points.map(_.rotateAroundCenterLeft(findRotationCenter())))

  def rotateRight(): Stone =
    if (points.isEmpty) this
    else copy(points = points.map(_.rotateAroundCenterRight(findRotationCenter())))

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
      copy(points = points.map(_ - Point(xDiff, min.y)))
    }

  def doesCollide(other: Stone): Boolean = points.exists(a => other.points.contains(a))

  def isInFrame(frame: Size): Boolean = points.forall(_.isInFrame(frame))

  def isOnTop: Boolean = points.exists(_.isOnTop)
}
