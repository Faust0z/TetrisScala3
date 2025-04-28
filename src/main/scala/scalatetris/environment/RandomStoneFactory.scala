package scalatetris.environment

import java.util.Random

/**
 * Objeto que genera piezas/Stones aleatoriamente.
 * Las piezas se crean en la posición inicial (0,0) y pueden rotarse aleatoriamente hasta 2 veces.
 * Se crean en la posición (0, 0) debido a que las piezas tienen diferentes tamaños y luego se centran.
 */
object RandomStoneFactory {
  private val random: Random = new Random()
  private val start: Point = Point(0, 0)

  /**
   * Crea y devuelve una pieza/piedra aleatoria con rotación aleatoria.
   */
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
