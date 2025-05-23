package scalatetris.ui

import scalatetris.components.{Point, Stone}

import scala.swing.TextArea

/**
 * Trait que define la interfaz para mostrar el estado del juego.
 *
 * Esta interfaz permite diferentes implementaciones de visualización,
 * como modo texto, modo gráfico, o incluso una versión sin salida visual.
 *
 * @see SwingDisplay Para una implementación en modo texto
 * @see DummyDisplay Para una implementación sin salida visual
 * @see TetrisPanel Para la implementación gráfica principal
 */
trait Display {
  /**
   * Renderiza el estado actual del juego.
   *
   * @param stones        Lista de piezas en el tablero (activas y fijas)
   * @param points        Lista de puntos ocupados en el tablero (para colisiones)
   * @param statistics    Estadísticas actuales del juego (puntuación, nivel, etc.)
   * @param isGameRunning Estado actual del juego (true si está activo, false si está en pausa o game over)
   * @return Unit No devuelve valor, solo actualiza la visualización
   * @note Este método se tiene que llamar cada vez que el estado del juego cambia
   */
  def render(stones: List[Stone], points: List[Point], statistics: Statistics, isGameRunning: Boolean): Unit
}

/**
 * Implementación de Display que muestra el juego en modo texto usando Swing.
 *
 * Esta implementación es útil para depuración o como visualización alternativa
 * cuando no se dispone de modo gráfico.
 *
 * @constructor Crea un nuevo SwingDisplay con un área de texto específica
 * @param area Área de texto de Swing donde se mostrará el juego
 * @note La visualización usa caracteres ASCII:
 *       - 'x' para bloques ocupados
 *       - ' ' para espacios vacíos
 *       - '|' y '-' para los bordes
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
 * por ejemplo, cuando se usa TetrisPanel para el renderizado gráfico.
 *
 * @note Esta implementación es utilizada por el sistema de actores
 *       cuando la visualización real se maneja a través de TetrisPanel
 */
class DummyDisplay extends Display {
  override def render(stones: List[Stone], points: List[Point], statistics: Statistics, isGameRunning: Boolean): Unit = {
    // No hace nada porque estamos usando un panel gráfico como TetrisPanel
  }
}
