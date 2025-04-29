package scalatetris.environment

import java.util.Random

/** 
 * Trait que define la interfaz para una fábrica de piezas de Tetris.
 * 
 * Esta interfaz permite crear diferentes implementaciones para generar piezas,
 * facilitando la creación de variantes del juego o modos de prueba.
 */
trait StoneFactory {
  /** 
   * Punto de inicio predeterminado para las nuevas piezas.
   * 
   * @return Punto en la coordenada (0,0)
   */
  protected def start: Point = Point(0, 0)

  /** 
   * Crea una nueva pieza aleatoria.
   * 
   * @return Nueva pieza de Tetris
   */
  def createRandomStone(): Stone
}

/** 
 * Implementación principal de StoneFactory que genera piezas aleatorias estándar de Tetris.
 * 
 * Esta fábrica crea las siete piezas clásicas del Tetris (I, O, T, L, J, S, Z)
 * y las rota aleatoriamente antes de devolverlas.
 */
object RandomStoneFactory extends StoneFactory {
  private val random: Random = new Random()

  /** 
   * Crea una nueva pieza aleatoria y la rota un número aleatorio de veces.
   * 
   * @return Nueva pieza aleatoria con rotación aleatoria
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

/** 
 * Fábrica que solo genera piezas de un solo punto.
 * 
 * Útil para pruebas o modos de juego especiales.
 */
object OnlyPointsStoneFactory extends StoneFactory {
  def createRandomStone(): Stone = Stone(start)
}

/** 
 * Fábrica que solo genera piezas cuadradas.
 * 
 * Útil para pruebas o modos de juego especiales.
 */
object OnlySquaresStoneFactory extends StoneFactory {
  def createRandomStone(): Stone = Square(start)
}

/** 
 * Fábrica que solo genera piezas en forma de línea.
 * 
 * Útil para pruebas o modos de juego especiales.
 */
object OnlyLinesStoneFactory extends StoneFactory {
  def createRandomStone(): Stone = Line(start)
}

/** 
 * Fábrica que genera piezas vacías (sin puntos).
 * 
 * Útil para pruebas o para desactivar la generación de nuevas piezas.
 */
object NoStonesFactory extends StoneFactory {
  def createRandomStone(): Stone = Stone(Nil)
}
