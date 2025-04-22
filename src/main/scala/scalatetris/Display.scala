package scalatetris

import scalatetris.environment.Statistics
import scalatetris.environment.{Point, Stone}
import scala.swing.TextArea

/**
 * Trait that defines the interface for displaying the game state.
 *
 * This interface allows for different visualization implementations,
 * such as text mode, graphic mode, or even a version without visual output.
 */
trait Display {
  /**
   * Renders the current game state.
   *
   * @param stones List of pieces on the board
   * @param points List of occupied points on the board
   * @param statistics Current game statistics
   * @param isGameRunning Current game state (true if active)
   */
  def render(stones: List[Stone], points: List[Point], statistics: Statistics, isGameRunning: Boolean): Unit
}

/**
 * Display implementation that displays the game in text mode using Swing.
 *
 * This implementation is useful for debugging or as an alternative display
 * when graphic mode is not available.
 *
 * @param area Text area where the game will be displayed
 */
class SwingDisplay(area: TextArea) extends Display {
  /**
   * Renders the game state in text mode.
   *
   * Displays an ASCII representation of the board where:
   * - 'x' represents an occupied block
   * - ' ' represents an empty space
   * - '|' and '-' represent the board's borders
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
 * Display implementation that does not perform any visualization.
 *
 * This implementation is used when visualization is handled externally,
 * for example, when TetrisPanel is used for graphical rendering.
 */
class DummyDisplay extends Display {
  override def render(stones: List[Stone], points: List[Point], statistics: Statistics, isGameRunning: Boolean): Unit = {
    // No hace nada porque estamos usando un panel gráfico como TetrisPanel
  }
}
