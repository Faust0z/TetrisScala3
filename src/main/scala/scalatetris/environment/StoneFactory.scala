package scalatetris.environment

import java.util.Random

trait StoneFactory {
  protected def start: Point = Point(0, 0)

  def createRandomStone(): Stone
}

object RandomStoneFactory extends StoneFactory {
  private val random: Random = new Random()

  def createRandomStone(): Stone = {
    val stone: Stone = random.nextInt(7) match {
      case 0 => Square(start)
      case 1 => Line(start)
      case 2 => WinnerPodium(start)
      case 3 => LetterLLeft(start)
      case 4 => LetterLRight(start)
      case 5 => StepLeft(start)
      case 6 => StepRight(start)
    }

    (1 to random.nextInt(3)).foldLeft(stone)((s, _) => s.rotateLeft())
  }
}

object OnlyPointsStoneFactory extends StoneFactory {
  def createRandomStone(): Stone = Stone(start)
}

object OnlySquaresStoneFactory extends StoneFactory {
  def createRandomStone(): Stone = Square(start)
}

object OnlyLinesStoneFactory extends StoneFactory {
  def createRandomStone(): Stone = Line(start)
}

object NoStonesFactory extends StoneFactory {
  def createRandomStone(): Stone = Stone(Nil)
}
