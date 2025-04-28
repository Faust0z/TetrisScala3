package scalatetris

import scalatetris.engine.GameEngine
import scalatetris.environment._

import scala.swing._
import java.awt.{Color, Graphics2D, GradientPaint, BasicStroke, GraphicsDevice, GraphicsEnvironment}

/** 
 * Panel que implementa la interfaz gráfica del juego Tetris.
 * 
 * Este panel maneja:
 * - El renderizado del tablero de juego
 * - La visualización de piezas y efectos
 * - Los paneles de información (estadísticas, siguiente pieza, hold)
 * - Estados especiales (pausa, game over)
 * - Efectos visuales y animaciones
 * 
 * @param engine Motor del juego que provee la lógica
 * @param initialBlockSize Tamaño inicial de cada bloque en píxeles
 */
class TetrisPanel(engine: GameEngine, initialBlockSize: Int = 30) extends Panel {
  preferredSize = new Dimension(640, 768)
  focusable = true
  requestFocus()

  /** Tamaño actual de cada bloque en píxeles */
  private var blockSize = initialBlockSize
  /** Desplazamiento horizontal del tablero */
  private var offsetX = 50
  /** Desplazamiento vertical del tablero */
  private var offsetY = 50

  /** 
   * Mapa de colores para las diferentes piezas.
   * Cada pieza tiene un color principal y un color de sombra.
   * Por ejemplo la pieza L tiene color naranja neón.
   */
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

  /** Colores del tema como:
   *  -Color de fondo, azul muy oscuro.
   *  -Color de la grilla, azul grisáceo.
   *  -Color de los bordes, azul brillante.
   *  -Color de la pieza fantasma, se le dio una tonalidad semi-transparente.
   */
  // Colores del tema
  private val backgroundColor = new Color(0, 0, 30)  // Azul muy oscuro
  private val gridColor = new Color(40, 40, 80)      // Azul grisáceo para la grilla
  private val borderColor = new Color(0, 191, 255)   // Azul brillante para bordes
  private val ghostColor = new Color(255, 255, 255, 40)  // Color semi-transparente para la pieza fantasma

  /** Estado previo del juego para detectar cambios */
  private var lastGameState: Boolean = true

  /** 
   * Recalcula tamaños y posiciones según el tamaño actual del panel.
   * Ajusta el tamaño de bloque y centra el tablero.
   */
  def resizeUI(): Unit = {
    val availableWidth = size.width
    val availableHeight = size.height
    
    // Calcular el nuevo tamaño de bloque basado en el espacio disponible
    val maxBlockWidth = (availableWidth * 0.70 / engine.boardSize.width).toInt
    val maxBlockHeight = (availableHeight * 0.85 / engine.boardSize.height).toInt
    blockSize = math.min(maxBlockWidth, maxBlockHeight)
    
    // Centrar la cuadrícula en el espacio disponible
    offsetX = (availableWidth - blockSize * engine.boardSize.width) / 2
    offsetY = (availableHeight - blockSize * engine.boardSize.height) / 2
  }

  /** 
   * Dibuja todos los componentes del juego.
   * Dibuja el tablero, el fondo,la grilla, bordes, pieza fantasma, las piezas, cuando se pausa, las estadisticas, la siguiente pieza, la pieza que se guarda y el game over al finalizar.
   * 
   * @param g Contexto gráfico donde dibujar
   */
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    // Configurar renderizado para mejor calidad
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(java.awt.RenderingHints.KEY_TEXT_ANTIALIASING, java.awt.RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    
    drawBackground(g)
    drawGrid(g)
    drawBorder(g)
    
    // Calcular y dibujar la pieza fantasma
    if (engine.isGameRunning && engine.stones.nonEmpty) {
      val ghostPiece = calculateGhostPiece(engine.stones.head)
      drawGhostPiece(g, ghostPiece)
    }
    
    drawStones(g)
    drawPauseReminder(g)
    // Dibujar los paneles de información después de todo lo demás
    drawHoldPanel(g)
    drawStatisticsPanel(g)
    drawPreviewPanel(g)

    if (!engine.boardIsRunning && engine.IsRunning) {
      drawGameOver(g)
    } else if (!engine.IsRunning) {
      drawPaused(g)
    }
  }

  /** 
   * Calcula la posición final de la pieza actual si cayera instantáneamente.
   * Esta función permite hacer los calculos del efecto fantasma, para ver donde caera la pieza que actualmente esta en juego.
   * 
   * @param currentStone Pieza actual
   * @return Nueva pieza en su posición final
   */
  private def calculateGhostPiece(currentStone: Stone): Stone = {
    var ghostStone = currentStone
    var testStone = ghostStone.moveDown()
    
    // Mover hacia abajo hasta que colisione
    while (testStone.isInFrame(engine.boardSize) && 
           !engine.stones.tail.exists(_.doesCollide(testStone))) {
      ghostStone = testStone
      testStone = ghostStone.moveDown()
    }
    
    ghostStone
  }

  /** 
   * Dibuja la proyección fantasma de la pieza actual.
   * Esta función permite visualizar el efecto fantasma previamente calculado.
   *
   * @param stone Pieza a dibujar como fantasma
   * @param g Contexto gráfico donde dibujar
   */
  private def drawGhostPiece(g: Graphics2D, stone: Stone): Unit = {
    g.setColor(ghostColor)
    stone.points.foreach { p =>
      val blockX = offsetX + p.x * blockSize
      val blockY = offsetY + p.y * blockSize
      g.fillRect(blockX, blockY, blockSize, blockSize)
      g.setColor(new Color(255, 255, 255, 60))
      g.drawRect(blockX, blockY, blockSize, blockSize)
    }
  }

  /** 
   * Dibuja el fondo degradado del juego.
   * 
   * @param g Contexto gráfico donde dibujar
   */
  private def drawBackground(g: Graphics2D): Unit = {
    val gradient = new GradientPaint(0, 0, backgroundColor, size.width, size.height, new Color(0, 0, 50))
    g.setPaint(gradient)
    g.fillRect(0, 0, size.width, size.height)
  }

  /** 
   * Dibuja la grilla del tablero de juego con los limites previamente establecido para que no sobresalga del fondo.
   * 
   * @param g Contexto gráfico donde dibujar
   */
  private def drawGrid(g: Graphics2D): Unit = {
    g.setColor(gridColor)
    // Líneas verticales
    for (x <- 0 to engine.boardSize.width)
      g.drawLine(offsetX + x * blockSize, offsetY, offsetX + x * blockSize, offsetY + engine.boardSize.height * blockSize)
    // Líneas horizontales
    for (y <- 0 to engine.boardSize.height)
      g.drawLine(offsetX, offsetY + y * blockSize, offsetX + engine.boardSize.width * blockSize, offsetY + y * blockSize)
  }

  /** 
   * Dibuja el borde del tablero de juego por los limites previamente establecidos.
   * 
   * @param g Contexto gráfico donde dibujar
   */
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

  /** 
   * Dibuja todas las piezas en el tablero que estan guardadas en la lista de piezas del engine el cual ya fueron colocadas en el tablero.
   * 
   * @param g Contexto gráfico donde dibujar
   */
  private def drawStones(g: Graphics2D): Unit = {
    val stones = engine.stones
    stones.foreach { stone =>
      val (mainColor, shadowColor) = stoneColors.getOrElse(stone.stoneType, (Color.GRAY, Color.DARK_GRAY))
      stone.points.foreach(p => fillBlock(g, p.x, p.y, mainColor, shadowColor))
    }
  }

  /** 
   * Dibuja un bloque individual con efectos visuales.
   * Esta función permite rellenar el bloque del color indicado previamente en "stoneColors", con su respectiva sombra y brillo.
   *
   * @param g Contexto gráfico donde dibujar
   * @param x Coordenada X del bloque
   * @param y Coordenada Y del bloque
   * @param mainColor Color principal del bloque
   * @param shadowColor Color de sombra del bloque
   */
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

  /** 
   * Dibuja el panel de estadísticas.
   * Esta compuesto por un titulo y un recuadro donde van a estar todas las estadisticas importantes que se guardan en enviroment --> Statistics.
   * Las estadisticas a mostrar son el nivel, las filas completadas denominadas simplemente filas, el tiempo transcurrido y los puntos obtenidos que fueron calculadas según el tiempo transcurrido y las filas completadas.
   * Cada una de estas estadistica va a ser una linea del panel de estadisticas y estará renderizada con la función "drawStatLine" definicas dentro de esta función.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawStatisticsPanel(g: Graphics2D): Unit = {
    val stats = engine.statistics
    val statX = offsetX + engine.boardSize.width * blockSize + 30
    var statY = offsetY + 20  // Ajustado para empezar más arriba

    val scaleFactor = blockSize / 30.0  // Factor de escala basado en el tamaño del bloque
    val panelWidth = (160 * scaleFactor).toInt
    val panelHeight = (150 * scaleFactor).toInt
    
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
    val titleFontSize = (20 * scaleFactor).toInt
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, titleFontSize))
    val titleText = "ESTADÍSTICAS"
    val fm = g.getFontMetrics
    val titleWidth = fm.stringWidth(titleText)
    g.drawString(titleText, statX + (panelWidth - titleWidth)/2 - 10, statY - 15)

    // Dibujar estadísticas
    val fontSize = (18 * scaleFactor).toInt
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, fontSize))
    statY += (10 * scaleFactor).toInt // Ajuste después del título

    /**
     * Dibuja cada linea del recuadro del panel de estadísticas.
     * Cada una de estas estadistica va a ser una linea del panel de estadisticas y estará renderizada por esta función.
     * En esta función se indica como va a ser el estilo de cada estadistica, con los colores, sombra y tipografía.
     *
     * @param g Contexto gráfico donde dibujar
     */
    def drawStatLine(label: String, value: String): Unit = {
      // Sombra
      g.setColor(new Color(0, 0, 0))
      g.drawString(label, statX + 1, statY + 1)
      
      val valueX = statX + panelWidth - (60 * scaleFactor).toInt  // Alineación derecha para los valores
      g.drawString(value, valueX + 1, statY + 1)
      
      // Texto principal
      g.setColor(Color.WHITE)
      g.drawString(label, statX, statY)
      g.setColor(new Color(0, 255, 255))  // Color cyan para los valores
      g.drawString(value, valueX, statY)
      
      statY += (30 * scaleFactor).toInt  // Espaciado entre líneas
    }

    val now = new java.util.Date()
    val duration = now.getTime - stats.startTime.getTime
    val seconds = (duration / 1000) % 60
    val minutes = (duration / 1000) / 60

    drawStatLine("NIVEL", engine.getLevel.toString)
    drawStatLine("FILAS", stats.rowsCompleted.toString)
    drawStatLine("TIEMPO", f"$minutes%02d:$seconds%02d")
    var scoreText = stats.score.toString
    if (stats.pendingScore > 0) {
      scoreText += f" +${stats.pendingScore}"
    }
    drawStatLine("PUNTOS", scoreText)
  }

  /** 
   * Dibuja el panel de preview de la siguiente pieza.
   * En esta función se muestra en cuadro la siguiente pieza que se va a crear para darle al jugador mas información para su estrategia.
   * Este panel cuenta con la estetica del titulo, el recuadro donde va a ser mostrada la pieza, la renderización de la pieza de manera centrada y demás detalles esteticos.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawPreviewPanel(g: Graphics2D): Unit = {
    val previewStone = engine.nextStone
    val (mainColor, shadowColor) = stoneColors.getOrElse(previewStone.stoneType, (Color.GRAY, Color.DARK_GRAY))

    val scaleFactor = blockSize / 30.0
    val panelWidth = (150 * scaleFactor).toInt
    val panelHeight = (150 * scaleFactor).toInt
    val previewX = offsetX + engine.boardSize.width * blockSize + 30
    val previewY = offsetY + (200 * scaleFactor).toInt

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
    val titleFontSize = (20 * scaleFactor).toInt
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, titleFontSize))
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

  /** 
   * Dibuja un bloque en el panel de preview.
   * Esta función permite pintar el color de la pieza que se encuentra en el cuadro de siguiente pieza.
   * 
   * @param g Contexto gráfico donde dibujar
   * @param x Coordenada X del bloque
   * @param y Coordenada Y del bloque
   * @param size Tamaño del bloque
   * @param mainColor Color principal del bloque
   * @param shadowColor Color de sombra del bloque
   */
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

  /** 
   * Dibuja el recordatorio de pausa.
   * En esta función se mostrara por debajo del tablero de juego indicando que con la tecla "P" podra poner en pausa el juego y ver los controles.
   * 
   * @param g Contexto gráfico donde dibujar
   */
  private def drawPauseReminder(g: Graphics2D): Unit = {
    if (engine.isGameRunning) {
      val reminderText = "Pausa/Controles: 'P'"
      val fontSize = (14 * blockSize / 30.0).toInt
      g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, fontSize))
      val fm = g.getFontMetrics
      val textWidth = fm.stringWidth(reminderText)
      
      val x = offsetX + (engine.boardSize.width * blockSize - textWidth) / 2
      val y = offsetY + engine.boardSize.height * blockSize + (25 * blockSize / 30.0).toInt
      
      // Sombra del texto
      g.setColor(Color.BLACK)
      g.drawString(reminderText, x + 1, y + 1)
      
      // Texto principal con color neón
      g.setColor(new Color(0, 255, 255))
      g.drawString(reminderText, x, y)
    }
  }

  /** 
   * Dibuja la pantalla de game over.
   * La pantalla del game over aparece cuando el jugador pierde, esta le permite ver la puntuación que realizo en esta partida y el record de la sesión, ya que sino se cierra el juego y comienza con la "R" una nueva partida puede recordad el mayor puntaje de todas las partidas que jugo en la misma sesión sin cerrar el juego.
   * Muestra el mensaje de game over, la tecla con la cual se reinicia para volver a jugar, el puntaje que hizo y el puntaje máximo.
   * Tambien se declara el estilo completo de la información dicha anteriormente, ya sea colores, tipografía y posicionamiento de los elementos.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawGameOver(g: Graphics2D): Unit = {
    val mainMessage = "GAME OVER"
    val scoreMessage = s"PUNTUACIÓN: ${engine.statistics.score}"
    val highScoreMessage = s"RÉCORD: ${Main.getHighScore}"
    val restartMessage = "Presiona R para reiniciar"
    
    // Actualizar el récord si es necesario
    Main.updateHighScore(engine.statistics.score)
    
    // Fondo semitransparente
    g.setColor(new Color(0, 0, 0, 180))
    g.fillRect(0, 0, size.width, size.height)
    
    // Configuración para el texto principal
    val titleFontSize = (48 * blockSize / 30.0).toInt
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, titleFontSize))
    val fmMain = g.getFontMetrics
    val mainMsgWidth = fmMain.stringWidth(mainMessage)
    val mainMsgHeight = fmMain.getHeight()
    
    // Dibujar el texto principal con efecto retro
    val xMain = (size.width - mainMsgWidth) / 2
    val yMain = (size.height - mainMsgHeight) / 2 - titleFontSize
    
    // Efecto de sombra/borde para estilo retro
    g.setColor(Color.BLACK)
    g.drawString(mainMessage, xMain + 3, yMain + 3)
    g.setColor(new Color(255, 50, 50)) // Rojo brillante
    g.drawString(mainMessage, xMain, yMain)
    
    // Configuración para el texto de puntuación
    val scoreFontSize = (24 * blockSize / 30.0).toInt
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, scoreFontSize))
    val fmScore = g.getFontMetrics
    val scoreMsgWidth = fmScore.stringWidth(scoreMessage)
    
    // Dibujar la puntuación
    val xScore = (size.width - scoreMsgWidth) / 2
    val yScore = yMain + mainMsgHeight + scoreFontSize
    
    g.setColor(Color.BLACK)
    g.drawString(scoreMessage, xScore + 2, yScore + 2)
    g.setColor(new Color(255, 215, 0)) // Dorado
    g.drawString(scoreMessage, xScore, yScore)
    
    // Dibujar el récord
    val highScoreMsgWidth = fmScore.stringWidth(highScoreMessage)
    val xHighScore = (size.width - highScoreMsgWidth) / 2
    val yHighScore = yScore + (scoreFontSize * 1.5).toInt
    
    g.setColor(Color.BLACK)
    g.drawString(highScoreMessage, xHighScore + 2, yHighScore + 2)
    g.setColor(new Color(0, 255, 255)) // Cyan
    g.drawString(highScoreMessage, xHighScore, yHighScore)
    
    // Mensaje para reiniciar
    val restartFontSize = (18 * blockSize / 30.0).toInt
    g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, restartFontSize))
    val fmRestart = g.getFontMetrics
    val restartMsgWidth = fmRestart.stringWidth(restartMessage)
    
    // Posicionar debajo de la puntuación
    val xRestart = (size.width - restartMsgWidth) / 2
    val yRestart = yHighScore + (scoreFontSize * 2).toInt
    
    g.setColor(Color.WHITE)
    g.drawString(restartMessage, xRestart, yRestart)
  }

  /** 
   * Obtiene el color correspondiente a una pieza.
   * 
   * @param stone Pieza de la cual obtener el color
   * @return Color asignado a la pieza
   */
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

  /** 
   * Dibuja la pantalla de controles.
   * Esta función nos permite mostrar los controles una vez que se pausa el juego, así el jugador puede tener a su disposicion toda la información necesaria para jugar correctamente.
   * En esta función esta el estilo completo de la lista de controles para poder ser renderizada correctamente con una linea estetica.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawControls(g: Graphics2D): Unit = {
    val controlsX = (size.width - 300) / 2  // Centrado en la pantalla
    var controlsY = (size.height - 400) / 2  // Comenzar más arriba en la pantalla

    val scaleFactor = blockSize / 30.0
    val keyWidth = (30 * scaleFactor).toInt
    val keyHeight = (25 * scaleFactor).toInt
    val spacing = (40 * scaleFactor).toInt
    val fontSize = (16 * scaleFactor).toInt

    /**
     * Define cada linea de la lista de controles.
     * Esta función contiene el estilo de cada linea de la lista de controles que se va a mostrar.
     *
     * @param g Contexto gráfico donde dibujar
     */
    def drawControlLine(key: String, action: String, y: Int): Unit = {
      // Dibujar el fondo del botón para la tecla
      g.setColor(new Color(40, 40, 40))
      val keyX = controlsX
      g.fillRect(keyX, y - keyHeight + 5, keyWidth, keyHeight)
      
      // Borde del botón
      g.setColor(new Color(80, 80, 80))
      g.drawRect(keyX, y - keyHeight + 5, keyWidth, keyHeight)
      
      // Dibujar la tecla
      g.setColor(Color.WHITE)
      g.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, fontSize))
      val keyMetrics = g.getFontMetrics
      val keyTextX = keyX + (keyWidth - keyMetrics.stringWidth(key)) / 2
      g.drawString(key, keyTextX, y)
      
      // Dibujar la acción
      g.setColor(new Color(200, 200, 200))
      g.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, fontSize))
      g.drawString(action, keyX + keyWidth + (20 * scaleFactor).toInt, y)
    }

    // Título "CONTROLES"
    g.setColor(new Color(0, 0, 0, 180))
    g.fillRect(0, 0, size.width, size.height)
    
    val titleFontSize = (36 * scaleFactor).toInt
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, titleFontSize))
    val titleText = "CONTROLES"
    val fm = g.getFontMetrics
    val titleWidth = fm.stringWidth(titleText)
    
    // Efecto de sombra para el título
    g.setColor(Color.BLACK)
    g.drawString(titleText, (size.width - titleWidth) / 2 + 2, controlsY - (40 * scaleFactor).toInt + 2)
    g.setColor(new Color(0, 191, 255)) // Deep Sky Blue
    g.drawString(titleText, (size.width - titleWidth) / 2, controlsY - (40 * scaleFactor).toInt)

    // Línea decorativa bajo el título
    g.setColor(new Color(0, 191, 255, 150))
    g.fillRect((size.width - titleWidth) / 2, controlsY - (20 * scaleFactor).toInt, titleWidth, 2)
    
    // Lista de controles
    drawControlLine("A", "Mover izquierda", controlsY + spacing)
    drawControlLine("D", "Mover derecha", controlsY + spacing * 2)
    drawControlLine("S", "Acelerar caída", controlsY + spacing * 3)
    drawControlLine("Q", "Rotar izquierda", controlsY + spacing * 4)
    drawControlLine("E", "Rotar derecha", controlsY + spacing * 5)
    drawControlLine("P", "Pausa", controlsY + spacing * 6)
    drawControlLine("C", "Continuar", controlsY + spacing * 7)
    drawControlLine("R", "Reiniciar juego", controlsY + spacing * 8)
    drawControlLine("M", "Volver al menú", controlsY + spacing * 9)
    drawControlLine("ESC", "Volver al menú", controlsY + spacing * 10)
    drawControlLine("F11", "Pantalla completa", controlsY + spacing * 11)
  }

  /** 
   * Dibuja la pantalla de pausa.
   * Esta función permite dibujar la pantalla de pausa la cual tiene un titulo superior de pausa y también muestra la lista de controles.
   * Contiene el estilo completo de todos los elementos que compone a la pantalla de pausa.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawPaused(g: Graphics2D): Unit = {
    val message = "PAUSA"
    
    // Fondo semitransparente
    g.setColor(new Color(0, 0, 0, 180))
    g.fillRect(0, 0, size.width, size.height)

    // Configuración para el texto "PAUSA"
    val scaleFactor = blockSize / 30.0
    val fontSize = (48 * scaleFactor).toInt
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, fontSize))
    val fm = g.getFontMetrics
    val msgWidth = fm.stringWidth(message)
    val msgHeight = fm.getHeight
    val x = (size.width - msgWidth) / 2
    val y = msgHeight + (50 * scaleFactor).toInt

    // Efecto de sombra/borde para el texto "PAUSA"
    g.setColor(Color.BLACK)
    g.drawString(message, x + 3, y + 3)
    g.setColor(new Color(0, 191, 255)) // Deep Sky Blue
    g.drawString(message, x, y)

    // Dibujar los controles
    drawControls(g)
  }

  /** 
   * Dibuja el panel de hold (pieza guardada).
   *
   * Esta función permite mostrar en pantalla la pieza que fue guardada por el juegador a la espera de ser invocada nuevamente cuando el jugador la requiera.
   * Contiene los estilos completos de cada elemento que aparece en el holdPanel.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawHoldPanel(g: Graphics2D): Unit = {
    val holdStone = engine.getHoldStone
    
    val scaleFactor = blockSize / 30.0
    val panelWidth = (150 * scaleFactor).toInt
    val panelHeight = (150 * scaleFactor).toInt
    val holdX = offsetX - panelWidth - 30
    val holdY = offsetY + 20

    // Fondo del panel con gradiente
    val gradientBg = new GradientPaint(
      holdX, holdY, new Color(0, 0, 40),
      holdX + panelWidth, holdY + panelHeight, new Color(0, 0, 60)
    )
    g.setPaint(gradientBg)
    g.fillRect(holdX, holdY, panelWidth, panelHeight)
    
    // Borde neón
    g.setColor(borderColor)
    g.setStroke(new BasicStroke(2))
    g.drawRect(holdX, holdY, panelWidth, panelHeight)

    // Título "HOLD"
    val titleFontSize = (20 * scaleFactor).toInt
    g.setFont(new java.awt.Font("Impact", java.awt.Font.BOLD, titleFontSize))
    g.setColor(Color.WHITE)
    g.drawString("HOLD", holdX + 10, holdY - 10)

    // Dibujar la pieza guardada si existe
    holdStone.foreach { stone =>
      val (mainColor, shadowColor) = stoneColors.getOrElse(stone.stoneType, (Color.GRAY, Color.DARK_GRAY))
      
      // Centrar la pieza hold en su panel
      val holdBlockSize = blockSize  // Mantener el mismo tamaño de bloque
      val pieceWidth = stone.points.map(_.x).max - stone.points.map(_.x).min + 1
      val pieceHeight = stone.points.map(_.y).max - stone.points.map(_.y).min + 1
      
      val centerX = holdX + (panelWidth - pieceWidth * holdBlockSize) / 2
      val centerY = holdY + (panelHeight - pieceHeight * holdBlockSize) / 2

      // Ajustar la posición de cada bloque para centrarlo
      val minX = stone.points.map(_.x).min
      val minY = stone.points.map(_.y).min
      
      stone.points.foreach { p =>
        val adjustedX = centerX + (p.x - minX) * holdBlockSize
        val adjustedY = centerY + (p.y - minY) * holdBlockSize
        fillPreviewBlock(g, adjustedX, adjustedY, holdBlockSize, mainColor, shadowColor)
      }
    }
  }
}
