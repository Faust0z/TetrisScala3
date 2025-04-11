package scalatetris

import scalatetris.engine.GameEngine
import scalatetris.environment._

import scala.swing._
import java.awt.{Color, Graphics2D}

class TetrisPanel(engine: GameEngine) extends Panel {
  preferredSize = new Dimension(640, 768)
  focusable = true
  requestFocus()

  private val blockSize = 30
  private val offsetX = 50
  private val offsetY = 50

  private val stoneColors: Map[String, Color] = Map(
    "Square" -> Color.YELLOW,
    "Line" -> Color.CYAN,
    "T" -> Color.MAGENTA,
    "L" -> Color.ORANGE,
    "J" -> Color.BLUE,
    "S" -> Color.GREEN,
    "Z" -> Color.RED,
    "Default" -> Color.GRAY
  )

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    drawBackground(g)
    drawGrid(g)
    drawStones(g)
    drawStatistics(g)
    drawPreviewStone(g)
    drawControls(g)
    //se va a ejecutar cuando termine el juego
    if (!engine.boardIsRunning && engine.IsRunning) {
      drawGameOver(g)
    } else if (!engine.IsRunning) {
      drawPaused(g)
    }

  }

  private def drawBackground(g: Graphics2D): Unit = {
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, size.width, size.height)
  }

  private def drawGrid(g: Graphics2D): Unit = {
    g.setColor(Color.LIGHT_GRAY)
    for (x <- 0 to engine.boardSize.width)
      g.drawLine(offsetX + x * blockSize, offsetY, offsetX + x * blockSize, offsetY + engine.boardSize.height * blockSize)
    for (y <- 0 to engine.boardSize.height)
      g.drawLine(offsetX, offsetY + y * blockSize, offsetX + engine.boardSize.width * blockSize, offsetY + y * blockSize)
  }

  private def drawStones(g: Graphics2D): Unit = {
    val stones = engine.stones
    stones.foreach { stone =>
      val color = stoneColors.getOrElse(stone.stoneType, Color.GRAY)
      g.setColor(color)
      stone.points.foreach(p => fillBlock(g, p.x, p.y, color))
    }
  }

  private def fillBlock(g: Graphics2D, x: Int, y: Int, color: Color): Unit = {
    g.setColor(color)
    g.fillRect(offsetX + x * blockSize, offsetY + y * blockSize, blockSize, blockSize)

    g.setColor(Color.BLACK)
    g.drawRect(offsetX + x * blockSize, offsetY + y * blockSize, blockSize, blockSize)
  }

  private def drawStatistics(g: Graphics2D): Unit = {
    val stats = engine.statistics
    g.setColor(Color.black)
    g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16))

    val statX = offsetX + engine.boardSize.width * blockSize + 30
    var statY = offsetY

    def drawLine(text: String): Unit = {
      g.drawString(text, statX, statY)
      statY += 25
    }

    val now = new java.util.Date()
    val duration = now.getTime - stats.startTime.getTime
    val seconds = (duration / 1000) % 60
    val minutes = (duration / 1000) / 60

    drawLine("ESTADÍSTICAS")
    drawLine(s"Filas eliminadas: ${stats.rowsCompleted}")
    drawLine(f"Tiempo: $minutes%02d:$seconds%02d")
  }

  private def drawGameOver(g: Graphics2D): Unit = {
    val message = "GAME OVER"
    g.setColor(new Color(0, 0, 0, 150)) // fondo semitransparente
    g.fillRect(0, 0, size.width, size.height)

    g.setColor(Color.RED)
    g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16))

    val fm = g.getFontMetrics
    val msgWidth = fm.stringWidth(message)
    val msgHeight = fm.getAscent

    val x = (size.width - msgWidth) / 2
    val y = (size.height + msgHeight) / 2

    g.drawString(message, x, y)
  }

  private def colorOf(stone: Stone): Color = stone.stoneType match {
    case "Square" => Color.YELLOW
    case "Line" => Color.CYAN
    case "T" => Color.MAGENTA
    case "L" => Color.BLUE
    case "J" => Color.ORANGE
    case "S" => Color.GREEN
    case "Z" => Color.RED
    case _ => Color.GRAY // Para el tipo "Default" o cualquier otro
  }

  // Dibujar pieza preview
  private def drawPreviewStone(g: Graphics2D): Unit = {
    val previewStone = engine.nextStone
    val previewColor = colorOf(previewStone)

    // Coordenadas de inicio para mostrar la preview (ajustalas si querés moverla)
    val previewX = offsetX + engine.boardSize.width * blockSize + 120
    val previewY = offsetY + 150

    previewStone.points.foreach { p =>
      val px = previewX + p.x * blockSize
      val py = previewY + p.y * blockSize
      g.setColor(previewColor)
      g.fillRect(px, py, blockSize, blockSize)
      g.setColor(Color.BLACK)
      g.drawRect(px, py, blockSize, blockSize)
    }

    // Etiqueta "Siguiente"
    g.setColor(Color.BLACK)
    g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16))
    g.drawString("Siguiente:", previewX, previewY - 10)
  }

  private def drawControls(g: Graphics2D): Unit = {
    val controlsX = offsetX + engine.boardSize.width * blockSize + 30
    var controlsY = offsetY + 350

    def drawLine(text: String): Unit = {
      g.drawString(text, controlsX, controlsY)
      controlsY += 20
    }

    g.setColor(Color.black)
    g.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 14))

    drawLine("CONTROLES")
    drawLine("A Mover izquierda")
    drawLine("D Mover derecha")
    drawLine("S Acelerar caída")
    drawLine("Q Rotar izquierda")
    drawLine("E Rotar derecha")
    drawLine("P: Pausa")
    drawLine("C: Continuar")
    drawLine("R: Reiniciar juego")
  }

  private def drawPaused(g: Graphics2D): Unit = {
    val message = "PAUSA"
    g.setColor(new Color(0, 0, 0, 100))
    g.fillRect(0, 0, size.width, size.height)

    g.setColor(Color.BLUE)
    g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 18))

    val fm = g.getFontMetrics
    val msgWidth = fm.stringWidth(message)
    val msgHeight = fm.getAscent

    val x = (size.width - msgWidth) / 2
    val y = (size.height + msgHeight) / 2

    g.drawString(message, x, y)
  }
}
