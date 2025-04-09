package scalatetris.environment

object Stone {
  def apply(start: Point): Stone = Stone(List(start))
}

object Square {
  def apply(start: Point): Stone =
    Stone(List(start.moveRight(), start.moveDown(), start.moveDown().moveRight())
    )
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

case class Stone(val points: List[Point]) {
  
  def moveDown() = Stone(points.map{_.moveDown()})
  
  def moveLeft() = Stone(points.map{_.moveLeft()})
  
  def moveRight() = Stone(points.map{_.moveRight()})
  
  def rotateLeft(): Stone = {
    if (points.length == 0)
      return this;
    
    val center = findRotationCenter()
    Stone(points.map{_.rotateAroundCenterLeft(center)})
  }
    
  def rotateRight(): Stone = {
    if (points.length == 0)
      return this;
    
    val center = findRotationCenter()
    Stone(points.map{_.rotateAroundCenterRight(center)})
  }
  
  private def findRotationCenter() = {
    val (min, max) = points.foldLeft((points(0), points(0))) { 
      case ((min, max), point) => (min.min(point), max.max(point)) 
    }
    
    val roundUp = Point(0, 1)
    (max + min + roundUp) / 2
  }
  
  def toTopCenter(center: Point): Stone = {
    if (points.length == 0)
      return this;
    
    val min = points.foldLeft(points(0)) { 
      case (min, point) => min.min(point)
    }
    
    val stoneCenter = findRotationCenter()
    
    val xDiff = stoneCenter.x - center.x
    
    Stone(points.map{_ - Point(xDiff, min.y)})
  }
  
  def doesCollide(other: Stone) = this.points.exists{a => 
    other.points.exists(a == _)
  }
  
  def isInFrame(frame: Size) = points.forall(_.isInFrame(frame))
                                
  def isOnTop(): Boolean = points.exists(_.isOnTop())
  
}