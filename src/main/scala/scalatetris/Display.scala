package scalatetris

import scalatetris.environment.Statistics
import scalatetris.environment.{Point, Stone}
import scala.swing.TextArea

trait Display {
  def render(stones: List[Stone], points: List[Point], statistics: Statistics, isGameRunning: Boolean): Unit
}

class SwingDisplay(area: TextArea) extends Display {
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

class DummyDisplay extends Display {
  override def render(stones: List[Stone], points: List[Point], statistics: Statistics, isGameRunning: Boolean): Unit = {
    // No hace nada porque estamos usando un panel gráfico como TetrisPanel
  }
}
