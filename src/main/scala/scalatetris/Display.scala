package scalatetris

import scalatetris.environment.Statistics
import scalatetris.environment.{Point, Stone}
import scala.swing.TextArea

/**
 * Trait que define la interfaz para mostrar el estado del juego.
 *
 * Esta interfaz permite diferentes implementaciones de visualización,
 * como modo texto, modo gráfico o incluso una versión sin salida visual.
 */
trait Display {
  /**
   * Renderiza el estado actual del juego.
   *
   * @param stones Lista de piezas en el tablero
   * @param points Lista de puntos ocupados en el tablero
   * @param statistics Estadísticas actuales del juego
   * @param isGameRunning Estado actual del juego (true si está activo)
   */
  def render(stones: List[Stone], points: List[Point], statistics: Statistics, isGameRunning: Boolean): Unit
}

/**
 * Implementación de Display que muestra el juego en modo texto usando Swing.
 *
 * Esta implementación es útil para depuración o como una visualización alternativa
 * cuando el modo gráfico no está disponible.
 *
 * @param area Área de texto donde se mostrará el juego
 */
class SwingDisplay(area: TextArea) extends Display {
  /**
   * Renderiza el estado del juego en modo texto.
   *
   * Muestra una representación ASCII del tablero donde:
   * - 'x' representa un bloque ocupado
   * - ' ' representa un espacio vacío
   * - '|' y '-' representan los bordes del tablero
   */
  def render(stones: List[Stone], points: List[Point], statistics: Statistics, isGameRunning: Boolean): Unit = {
    // Versión textual provisional
    val boardText = {
      val width = 10
      val height = 20
      val pointSet = points.map(p => (p.x, p.y)).toSet
      val builder = new StringBuilder

      for (y <- 0 until height) {
        builder.append("|")
        for (x <- 0 until width) {
          builder.append(if (pointSet.contains((x, y))) "x" else " ")
        }
        builder.append("|\n")
      }
      builder.append("-" * (width + 2) + "\n")
      if (!isGameRunning) builder.append("GAME OVER\n")
      builder.toString()
    }
    area.text = boardText
  }
}

/**
 * Implementación de Display que no realiza ninguna visualización.
 *
 * Esta implementación se usa cuando la visualización se maneja externamente,
 * por ejemplo, cuando se utiliza TetrisPanel para el renderizado gráfico.
 */
class DummyDisplay extends Display {
  override def render(stones: List[Stone], points: List[Point], statistics: Statistics, isGameRunning: Boolean): Unit = {
    // No hace nada porque estamos usando un panel gráfico como TetrisPanel
  }
}
