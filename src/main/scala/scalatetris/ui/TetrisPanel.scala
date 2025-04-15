package scalatetris

import scalatetris.engine.GameEngine
import scalatetris.environment._

import scala.swing._
import java.awt.{Color, Graphics2D, GradientPaint, BasicStroke}

class TetrisPanel(engine: GameEngine) extends Panel {
  preferredSize = new Dimension(640, 768)
  focusable = true
  requestFocus()

  private val blockSize = 30
  private val offsetX = 50
  private val offsetY = 50

  // Colores retro para las piezas con efecto neón
  private val stoneColors: Map[String, (Color, Color)] = Map(
    "Square" -> (new Color(255, 255, 0), new Color(200, 200, 0)),    // Amarillo neón
    "Line" -> (new Color(0, 255, 255), new Color(0, 200, 200)),      // Cyan neón
    "T" -> (new Color(255, 0, 255), new Color(200, 0, 200)),         // Magenta neón
    "L" -> (new Color(255, 165, 0), new Color(200, 130, 0)),         // Naranja neón
    "J" -> (new Color(0, 128, 255), new Color(0, 100, 200)),         // Azul neón
    "S" -> (new Color(0, 255, 0), new Color(0, 200, 0)),             // Verde neón
    "Z" -> (new Color(255, 0, 0), new Color(200, 0, 0)),             // Rojo neón
    "Default" -> (Color.GRAY, Color.DARK_GRAY)
  )

  // Color de fondo del juego
  private val backgroundColor = new Color(0, 0, 30)  // Azul muy oscuro
  private val gridColor = new Color(40, 40, 80)      // Azul grisáceo para la grilla
  private val borderColor = new Color(0, 191, 255)   // Azul brillante para bordes

  private var lastGameState: Boolean = true // Inicialmente asumimos que el juego está corriendo

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    // Configurar renderizado para mejor calidad
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(java.awt.RenderingHints.KEY_TEXT_ANTIALIASING, java.awt.RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    
    drawBackground(g)
    drawGrid(g)
    drawBorder(g)
    drawStones(g)
    drawPauseReminder(g)
    // Dibujar los paneles de información después de todo lo demás
    drawStatisticsPanel(g)
    drawPreviewPanel(g)

    if (!engine.boardIsRunning && engine.IsRunning) {
      drawGameOver(g)
    } else if (!engine.IsRunning) {
      drawPaused(g)
    }
  }

  private def drawBackground(g: Graphics2D): Unit = {
    // Fondo degradado
    val gradient = new GradientPaint(0, 0, backgroundColor, size.width, size.height, new Color(0, 0, 50))
    g.setPaint(gradient)
    g.fillRect(0, 0, size.width, size.height)
  }

  private def drawGrid(g: Graphics2D): Unit = {
    g.setColor(gridColor)
    // Líneas verticales
    for (x <- 0 to engine.boardSize.width)
      g.drawLine(offsetX + x * blockSize, offsetY, offsetX + x * blockSize, offsetY + engine.boardSize.height * blockSize)
    // Líneas horizontales
    for (y <- 0 to engine.boardSize.height)
      g.drawLine(offsetX, offsetY + y * blockSize, offsetX + engine.boardSize.width * blockSize, offsetY + y * blockSize)
  }

  private def drawBorder(g: Graphics2D): Unit = {
    val borderWidth = 2
    g.setColor(borderColor)
    g.setStroke(new BasicStroke(borderWidth))
    g.drawRect(
      offsetX - borderWidth, 
      offsetY - borderWidth,
      engine.boardSize.width * blockSize + borderWidth * 2,
      engine.boardSize.height * blockSize + borderWidth * 2
    )
  }

  private def drawStones(g: Graphics2D): Unit = {
    val stones = engine.stones
    stones.foreach { stone =>
      val (mainColor, shadowColor) = stoneColors.getOrElse(stone.stoneType, (Color.GRAY, Color.DARK_GRAY))
      stone.points.foreach(p => fillBlock(g, p.x, p.y, mainColor, shadowColor))
    }
  }

  private def fillBlock(g: Graphics2D, x: Int, y: Int, mainColor: Color, shadowColor: Color): Unit = {
    val blockX = offsetX + x * blockSize
    val blockY = offsetY + y * blockSize
    
    // Sombra interior
    g.setColor(shadowColor)
    g.fillRect(blockX, blockY, blockSize, blockSize)
    
    // Color principal con efecto de brillo
    g.setColor(mainColor)
    g.fillRect(blockX + 2, blockY + 2, blockSize - 4, blockSize - 4)
    
    // Brillo en la esquina superior izquierda
    g.setColor(new Color(255, 255, 255, 100))
    g.fillRect(blockX + 2, blockY + 2, blockSize - 6, 2)
    g.fillRect(blockX + 2, blockY + 2, 2, blockSize - 6)
  }

  private def drawStatisticsPanel(g: Graphics2D): Unit = {
    val stats = engine.statistics
    val statX = offsetX + engine.boardSize.width * blockSize + 30
    var statY = offsetY + 20  // Ajustado para empezar más arriba

    // Panel de estadísticas con borde neón
    val panelWidth = 160
    val panelHeight = 120
    
    // Fondo del panel con gradiente
    val gradientBg = new GradientPaint(
      statX - 10, statY - 10, new Color(0, 0, 40),
      statX - 10 + panelWidth, statY - 10 + panelHeight, new Color(0, 0, 60)
    )
    g.setPaint(gradientBg)
    g.fillRect(statX - 10, statY - 10, panelWidth, panelHeight)
    
    // Borde neón
    g.setColor(borderColor)
    g.setStroke(new BasicStroke(2))
    g.drawRect(statX - 10, statY - 10, panelWidth, panelHeight)

    // Título del panel
    g.setColor(Color.WHITE)
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, 20))
    val titleText = "ESTADÍSTICAS"
    val fm = g.getFontMetrics
    val titleWidth = fm.stringWidth(titleText)
    g.drawString(titleText, statX + (panelWidth - titleWidth)/2 - 10, statY - 15)

    // Dibujar estadísticas
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, 18))
    statY += 10 // Ajuste después del título

    def drawStatLine(label: String, value: String): Unit = {
      // Sombra
      g.setColor(new Color(0, 0, 0))
      g.drawString(label, statX + 1, statY + 1)
      
      val valueX = statX + panelWidth - 60  // Alineación derecha para los valores
      g.drawString(value, valueX + 1, statY + 1)
      
      // Texto principal
      g.setColor(Color.WHITE)
      g.drawString(label, statX, statY)
      g.setColor(new Color(0, 255, 255))  // Color cyan para los valores
      g.drawString(value, valueX, statY)
      
      statY += 30  // Espaciado entre líneas
    }

    val now = new java.util.Date()
    val duration = now.getTime - stats.startTime.getTime
    val seconds = (duration / 1000) % 60
    val minutes = (duration / 1000) / 60

    drawStatLine("FILAS", stats.rowsCompleted.toString)
    drawStatLine("TIEMPO", f"$minutes%02d:$seconds%02d")
    var scoreText = stats.score.toString
    if (stats.pendingScore > 0) {
      scoreText += f" +${stats.pendingScore}"
    }
    drawStatLine("PUNTOS", scoreText)
  }

  private def drawPreviewPanel(g: Graphics2D): Unit = {
    val previewStone = engine.nextStone
    val (mainColor, shadowColor) = stoneColors.getOrElse(previewStone.stoneType, (Color.GRAY, Color.DARK_GRAY))

    val previewX = offsetX + engine.boardSize.width * blockSize + 30
    val previewY = offsetY + 200

    // Panel de preview con borde neón
    val panelWidth = 150
    val panelHeight = 150
    
    // Fondo del panel con gradiente
    val gradientBg = new GradientPaint(
      previewX, previewY, new Color(0, 0, 40),
      previewX + panelWidth, previewY + panelHeight, new Color(0, 0, 60)
    )
    g.setPaint(gradientBg)
    g.fillRect(previewX, previewY, panelWidth, panelHeight)
    
    // Borde neón
    g.setColor(borderColor)
    g.setStroke(new BasicStroke(2))
    g.drawRect(previewX, previewY, panelWidth, panelHeight)

    // Título "SIGUIENTE"
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, 20))
    g.setColor(Color.WHITE)
    g.drawString("SIGUIENTE", previewX + 10, previewY - 10)

    // Centrar la pieza preview en su panel
    val previewBlockSize = blockSize  // Mantener el mismo tamaño de bloque
    val pieceWidth = previewStone.points.map(_.x).max - previewStone.points.map(_.x).min + 1
    val pieceHeight = previewStone.points.map(_.y).max - previewStone.points.map(_.y).min + 1
    
    val centerX = previewX + (panelWidth - pieceWidth * previewBlockSize) / 2
    val centerY = previewY + (panelHeight - pieceHeight * previewBlockSize) / 2

    // Ajustar la posición de cada bloque para centrarlo
    val minX = previewStone.points.map(_.x).min
    val minY = previewStone.points.map(_.y).min
    
    previewStone.points.foreach { p =>
      val adjustedX = centerX + (p.x - minX) * previewBlockSize
      val adjustedY = centerY + (p.y - minY) * previewBlockSize
      fillPreviewBlock(g, adjustedX, adjustedY, previewBlockSize, mainColor, shadowColor)
    }
  }

  private def fillPreviewBlock(g: Graphics2D, x: Float, y: Float, size: Int, mainColor: Color, shadowColor: Color): Unit = {
    // Sombra interior
    g.setColor(shadowColor)
    g.fillRect(x.toInt, y.toInt, size, size)
    
    // Color principal con efecto de brillo
    g.setColor(mainColor)
    g.fillRect(x.toInt + 2, y.toInt + 2, size - 4, size - 4)
    
    // Brillo en la esquina superior izquierda
    g.setColor(new Color(255, 255, 255, 100))
    g.fillRect(x.toInt + 2, y.toInt + 2, size - 6, 2)
    g.fillRect(x.toInt + 2, y.toInt + 2, 2, size - 6)
  }

  private def drawPauseReminder(g: Graphics2D): Unit = {
    if (engine.isGameRunning) {
      val reminderText = "Pausa/Controles: 'P'"
      g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 14))
      val fm = g.getFontMetrics
      val textWidth = fm.stringWidth(reminderText)
      
      val x = offsetX + (engine.boardSize.width * blockSize - textWidth) / 2
      val y = offsetY + engine.boardSize.height * blockSize + 25
      
      // Sombra del texto
      g.setColor(Color.BLACK)
      g.drawString(reminderText, x + 1, y + 1)
      
      // Texto principal con color neón
      g.setColor(new Color(0, 255, 255))
      g.drawString(reminderText, x, y)
    }
  }

  private def drawGameOver(g: Graphics2D): Unit = {
    val mainMessage = "GAME OVER"
    val restartMessage = "Presiona R para reiniciar"
    
    // Fondo semitransparente
    g.setColor(new Color(0, 0, 0, 180))
    g.fillRect(0, 0, size.width, size.height)
    
    // Configuración para el texto principal
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, 48))
    val fmMain = g.getFontMetrics
    val mainMsgWidth = fmMain.stringWidth(mainMessage)
    val mainMsgHeight = fmMain.getHeight()
    
    // Dibujar el texto principal con efecto retro
    val xMain = (size.width - mainMsgWidth) / 2
    val yMain = (size.height - mainMsgHeight) / 2
    
    // Efecto de sombra/borde para estilo retro
    g.setColor(Color.BLACK)
    g.drawString(mainMessage, xMain + 3, yMain + 3)
    g.setColor(new Color(255, 50, 50)) // Rojo brillante
    g.drawString(mainMessage, xMain, yMain)
    
    // Mensaje para reiniciar
    g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 18))
    val fmRestart = g.getFontMetrics
    val restartMsgWidth = fmRestart.stringWidth(restartMessage)
    
    // Posicionar debajo del mensaje principal
    val xRestart = (size.width - restartMsgWidth) / 2
    val yRestart = yMain + mainMsgHeight + 20
    
    g.setColor(Color.WHITE)
    g.drawString(restartMessage, xRestart, yRestart)
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

  private def drawControls(g: Graphics2D): Unit = {
    val controlsX = (size.width - 300) / 2  // Centrado en la pantalla
    var controlsY = (size.height - 400) / 2  // Comenzar más arriba en la pantalla

    def drawControlLine(key: String, action: String, y: Int): Unit = {
      // Dibujar el fondo del botón para la tecla
      g.setColor(new Color(40, 40, 40))
      val keyX = controlsX
      val keyWidth = 30
      g.fillRect(keyX, y - 20, keyWidth, 25)
      
      // Borde del botón
      g.setColor(new Color(80, 80, 80))
      g.drawRect(keyX, y - 20, keyWidth, 25)
      
      // Dibujar la tecla
      g.setColor(Color.WHITE)
      g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16))
      val keyMetrics = g.getFontMetrics
      val keyTextX = keyX + (keyWidth - keyMetrics.stringWidth(key)) / 2
      g.drawString(key, keyTextX, y)
      
      // Dibujar la acción
      g.setColor(new Color(200, 200, 200))
      g.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 16))
      g.drawString(action, keyX + keyWidth + 20, y)
    }

    // Título "CONTROLES"
    g.setColor(new Color(0, 0, 0, 180))
    g.fillRect(0, 0, size.width, size.height)
    
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, 36))
    val titleText = "CONTROLES"
    val fm = g.getFontMetrics
    val titleWidth = fm.stringWidth(titleText)
    
    // Efecto de sombra para el título
    g.setColor(Color.BLACK)
    g.drawString(titleText, (size.width - titleWidth) / 2 + 2, controlsY - 40 + 2)
    g.setColor(new Color(0, 191, 255)) // Deep Sky Blue
    g.drawString(titleText, (size.width - titleWidth) / 2, controlsY - 40)

    // Línea decorativa bajo el título
    g.setColor(new Color(0, 191, 255, 150))
    g.fillRect((size.width - titleWidth) / 2, controlsY - 20, titleWidth, 2)

    // Espacio entre cada control
    val spacing = 40
    
    // Lista de controles
    drawControlLine("A", "Mover izquierda", controlsY + spacing)
    drawControlLine("D", "Mover derecha", controlsY + spacing * 2)
    drawControlLine("S", "Acelerar caída", controlsY + spacing * 3)
    drawControlLine("Q", "Rotar izquierda", controlsY + spacing * 4)
    drawControlLine("E", "Rotar derecha", controlsY + spacing * 5)
    drawControlLine("P", "Pausa", controlsY + spacing * 6)
    drawControlLine("C", "Continuar", controlsY + spacing * 7)
    drawControlLine("R", "Reiniciar juego", controlsY + spacing * 8)
  }

  private def drawPaused(g: Graphics2D): Unit = {
    val message = "PAUSA"
    
    // Fondo semitransparente
    g.setColor(new Color(0, 0, 0, 180))
    g.fillRect(0, 0, size.width, size.height)

    // Configuración para el texto "PAUSA"
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, 48))
    val fm = g.getFontMetrics
    val msgWidth = fm.stringWidth(message)
    val msgHeight = fm.getHeight
    val x = (size.width - msgWidth) / 2
    val y = msgHeight + 50

    // Efecto de sombra/borde para el texto "PAUSA"
    g.setColor(Color.BLACK)
    g.drawString(message, x + 3, y + 3)
    g.setColor(new Color(0, 191, 255)) // Deep Sky Blue
    g.drawString(message, x, y)

    // Dibujar los controles
    drawControls(g)
  }
}
