package scalatetris.environment

import java.util.Random

trait StoneFactory {
  protected def start = Point(0, 0) 
  
  def createRandomStone(): Stone
}

object RandomStoneFactory extends StoneFactory {
  private val random = new Random()
  
  def createRandomStone() = 
    {
      val rand = random.nextInt(7)
      var point = rand match {
        case 0 => Square(start)
        case 1 => Line(start)
        case 2 => WinnerPodium(start)
        case 3 => LetterLLeft(start)
        case 4 => LetterLRight(start)
        case 5 => StepLeft(start)
        case 6 => StepRight(start) 
      }
      
      val rotation = random.nextInt(3)
      for (_ <- 1 to rotation) {
        point = point.rotateLeft()
      }
      point
    }
}

object OnlyPointsStoneFactory extends StoneFactory {
   def createRandomStone() = Stone(start)
}

object OnlySquaresStoneFactory extends StoneFactory {
   def createRandomStone() = Square(start)
}

object OnlyLinesStoneFactory extends StoneFactory {
   def createRandomStone() = Line(start)
}

object NoStonesFactory extends StoneFactory {
   def createRandomStone() = Stone(Nil)
}