package scalatetris.ui

import scalatetris.engine.AudioManager

import java.awt.{BasicStroke, Color, Font, GradientPaint, Graphics2D}
import scala.swing.*
import scala.swing.event.*

/**
 * Panel que implementa el menú principal del juego.
 *
 * Este panel maneja:
 * - La interfaz del menú principal
 * - Los botones interactivos
 * - La pantalla de controles
 * - El control de volumen
 * - Efectos visuales y animaciones
 *
 * @param onStartGame Función a ejecutar cuando se inicia el juego
 * @param onQuit      Función a ejecutar cuando se sale del juego
 */
class MainMenuPanel(onStartGame: () => Unit, onQuit: () => Unit) extends Panel {

  /** Colores del tema del menú */
  private val backgroundColor = new Color(0, 0, 30) // Azul muy oscuro
  private val buttonColor = new Color(0, 0, 60) // Azul para botones
  private val highlightColor = new Color(0, 191, 255) // Cyan brillante
  private val titleColor = new Color(255, 50, 50) // Rojo brillante para el título

  /** Lista de botones del menú principal */
  private val buttons = List(
    new MenuButton("JUGAR", 0),
    new MenuButton("CONTROLES", 1),
    new MenuButton("SALIR", 2)
  )

  /** Índice del botón sobre el que está el cursor, si hay alguno */
  private var hoveredButton: Option[Int] = None
  /** Indica si se está mostrando la pantalla de controles */
  private var showControls: Boolean = false
  /** Indica si el cursor está sobre el botón de volver al menú */
  private var backButtonHovered: Boolean = false

  /** Rectángulo que define el área del control de volumen */
  private var volumeSliderRect: Rectangle = new Rectangle(0, 0, 0, 0)
  /** Indica si se está arrastrando el control de volumen */
  private var isDraggingVolume: Boolean = false
  /** Posición actual del control de volumen (0-100) */
  private var volumeHandlePosition: Int = (AudioManager.getVolume * 100).toInt

  listenTo(mouse.clicks, mouse.moves, keys)

  /**
   * Manejador de eventos del panel.
   * Procesa eventos de mouse y teclado para:
   * - Detectar hover sobre botones
   * - Manejar clics en botones
   * - Controlar el volumen
   * - Manejar la tecla ESC
   */
  reactions += {
    case e: MouseMoved =>
      updateHoveredButton(e.point)
      if (showControls) {
        val panelWidth = math.min(size.width - 100, 600)
        val panelHeight = size.height - 200
        val panelX = (size.width - panelWidth) / 2
        val panelY = 100
        val backButtonWidth = 200
        val backButtonHeight = 40
        val backButtonX = panelX + (panelWidth - backButtonWidth) / 2
        val backButtonY = panelY + panelHeight - 60

        backButtonHovered = e.point.x >= backButtonX && e.point.x <= (backButtonX + backButtonWidth) &&
          e.point.y >= backButtonY && e.point.y <= (backButtonY + backButtonHeight)
      }
      repaint()

    case e: MouseDragged =>
      if (!showControls && volumeSliderRect.contains(e.point)) {
        isDraggingVolume = true
        val sliderX = e.point.x - volumeSliderRect.x
        val clampedX = math.max(0, math.min(sliderX, volumeSliderRect.width))
        volumeHandlePosition = clampedX * 100 / volumeSliderRect.width
        AudioManager.setVolume(volumeHandlePosition / 100.0f)
        repaint()
      }

    case e: KeyPressed =>
      if (showControls && e.key == Key.Escape) {
        showControls = false
        repaint()
      }

    case e: MouseClicked =>
      if (showControls) {
        val panelWidth = math.min(size.width - 100, 600)
        val panelHeight = size.height - 200
        val panelX = (size.width - panelWidth) / 2
        val panelY = 100
        val backButtonWidth = 200
        val backButtonHeight = 40
        val backButtonX = panelX + (panelWidth - backButtonWidth) / 2
        val backButtonY = panelY + panelHeight - 60

        if (e.point.x >= backButtonX && e.point.x <= (backButtonX + backButtonWidth) &&
          e.point.y >= backButtonY && e.point.y <= (backButtonY + backButtonHeight)) {
          showControls = false
          repaint()
        }
      } else if (volumeSliderRect.contains(e.point)) {
        val sliderX = e.point.x - volumeSliderRect.x
        volumeHandlePosition = sliderX * 100 / volumeSliderRect.width
        AudioManager.setVolume(volumeHandlePosition / 100.0f)
        repaint()
      } else {
        hoveredButton match {
          case Some(0) => onStartGame()
          case Some(1) =>
            showControls = true
            repaint()
          case Some(2) => onQuit()
          case _ =>
        }
      }

      isDraggingVolume = false
  }

  /**
   * Actualiza el botón sobre el que está el cursor.
   *
   * @param point Posición actual del cursor
   */
  private def updateHoveredButton(point: Point): Unit = {
    hoveredButton = buttons.find(_.contains(point)).map(_.index)
  }

  /**
   * Dibuja todos los componentes del panel.
   *
   * @param g Contexto gráfico donde dibujar
   */
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    // Mejora la calidad más que nada de los textos
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(java.awt.RenderingHints.KEY_TEXT_ANTIALIASING, java.awt.RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    val gradient = new GradientPaint(0, 0, backgroundColor, size.width, size.height, new Color(0, 0, 50))
    g.setPaint(gradient)
    g.fillRect(0, 0, size.width, size.height)

    if (showControls) {
      drawControls(g)
    } else {
      drawLogo(g)
      drawButtons(g)
      drawVolumeSlider(g)
    }
  }

  /**
   * Dibuja el logo del juego con efectos visuales.
   * Incluye efectos de sombra, colores neón y bloques estilo Tetris.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawLogo(g: Graphics2D): Unit = {
    val title = "TETRIS"

    val titleSize = math.min(size.width / 8, 100)
    g.setFont(new Font("Impact", Font.BOLD, titleSize))

    val fm = g.getFontMetrics
    val titleWidth = fm.stringWidth(title + 2)
    val x = (size.width - titleWidth) / 2
    val y = size.height / 4

    g.setColor(Color.BLACK)
    g.drawString(title, x + 4, y + 4)

    val letters = title.toCharArray

    val totalLetterWidth = letters.map(letter => fm.charWidth(letter)).sum

    val extraSpace = (titleWidth - totalLetterWidth) / (letters.length - 1)

    var currentX = x

    for (i <- letters.indices) {
      val letterWidth = fm.charWidth(letters(i))

      val letterColor = i match {
        case 0 => new Color(255, 0, 0) // T
        case 1 => new Color(255, 165, 0) // E
        case 2 => new Color(0, 255, 255) // T
        case 3 => new Color(0, 255, 0) // R
        case 4 => new Color(255, 0, 255) // I
        case 5 => new Color(255, 255, 0) // S
        case _ => Color.WHITE
      }
      val letterY = if (i == 4) y - 2 else y

      g.setColor(letterColor)
      g.drawString(letters(i).toString, currentX, letterY)

      g.setStroke(new BasicStroke(2))
      g.drawRect(currentX - 2, y - titleSize + 10, letterWidth + 4, titleSize)

      currentX += letterWidth + extraSpace
    }

    // Subtítulo
    g.setFont(new Font("Arial", Font.PLAIN, titleSize / 3))
    val subtitle = "SCALA EDITION"
    val subtitleWidth = g.getFontMetrics.stringWidth(subtitle)
    g.setColor(Color.WHITE)
    g.drawString(subtitle, (size.width - subtitleWidth) / 2, y + titleSize / 2)
  }

  /**
   * Dibuja los botones del menú principal con efectos visuales.
   * Incluye efectos hover, sombras y bordes brillantes.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawButtons(g: Graphics2D): Unit = {
    val buttonWidth = math.min(size.width / 3, 300)
    val buttonHeight = 60
    val startY = size.height / 2
    val spacing = buttonHeight + 20

    buttons.foreach { button =>
      val y = startY + button.index * spacing
      button.update((size.width - buttonWidth) / 2, y, buttonWidth, buttonHeight)

      g.setColor(if (hoveredButton.contains(button.index)) buttonColor.brighter else buttonColor)
      g.fillRect(button.x, button.y, button.width, button.height)

      g.setColor(if (hoveredButton.contains(button.index)) highlightColor else Color.GRAY)
      g.setStroke(new BasicStroke(3))
      g.drawRect(button.x, button.y, button.width, button.height)

      val buttonFontSize = buttonHeight / 2
      g.setFont(new Font("Impact", Font.BOLD, buttonFontSize))
      val fm = g.getFontMetrics
      val textWidth = fm.stringWidth(button.text)
      val textX = button.x + (button.width - textWidth) / 2
      val textY = button.y + (button.height + buttonFontSize) / 2

      g.setColor(Color.BLACK)
      g.drawString(button.text, textX + 2, textY + 2)

      g.setColor(if (hoveredButton.contains(button.index)) highlightColor else Color.WHITE)
      g.drawString(button.text, textX, textY)
    }
  }

  /**
   * Dibuja la pantalla de controles del juego.
   * Muestra una lista de todas las teclas y sus funciones,
   * con efectos visuales y un botón para volver al menú.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawControls(g: Graphics2D): Unit = {
    val panelWidth = math.min(size.width - 100, 600)
    val panelHeight = size.height - 200
    val panelX = (size.width - panelWidth) / 2
    val panelY = 100

    g.setColor(new Color(0, 0, 40, 220))
    g.fillRect(panelX, panelY, panelWidth, panelHeight)
    g.setColor(highlightColor)
    g.setStroke(new BasicStroke(3))
    g.drawRect(panelX, panelY, panelWidth, panelHeight)

    // Título
    val titleFontSize = 40
    g.setFont(new Font("Impact", Font.BOLD, titleFontSize))
    val title = "CONTROLES"
    val fm = g.getFontMetrics
    val titleWidth = fm.stringWidth(title)
    g.setColor(Color.BLACK)
    g.drawString(title, panelX + (panelWidth - titleWidth) / 2 + 2, panelY + 50 + 2)
    g.setColor(highlightColor)
    g.drawString(title, panelX + (panelWidth - titleWidth) / 2, panelY + 50)

    val controls = List(
      ("A", "Mover izquierda"),
      ("D", "Mover derecha"),
      ("S", "Acelerar caída"),
      ("Q", "Rotar izquierda"),
      ("E", "Rotar derecha"),
      ("H", "Guardar pieza (Hold)"),
      ("P", "Pausa"),
      ("C", "Continuar"),
      ("R", "Reiniciar juego"),
      ("ESC", "Volver al menú"),
      ("F11", "Pantalla completa (in-game)")
    )
    val fontSize = 20
    g.setFont(new Font("Arial", Font.BOLD, fontSize))
    var yPos = panelY + 120
    val spacing = 40
    val keyWidth = 50

    controls.foreach { case (key, action) =>
      g.setColor(new Color(40, 40, 40))
      g.fillRect(panelX + 50, yPos - fontSize, keyWidth, fontSize + 10)
      g.setColor(new Color(100, 100, 100))
      g.setStroke(new BasicStroke(1))
      g.drawRect(panelX + 50, yPos - fontSize, keyWidth, fontSize + 10)
      g.setColor(Color.WHITE)
      g.drawString(key, panelX + 50 + (keyWidth - g.getFontMetrics.stringWidth(key)) / 2, yPos)
      g.setColor(Color.LIGHT_GRAY)
      g.drawString(action, panelX + 50 + keyWidth + 20, yPos)

      yPos += spacing
    }

    val backButtonWidth = 200
    val backButtonHeight = 40
    val backButtonX = panelX + (panelWidth - backButtonWidth) / 2
    val backButtonY = panelY + panelHeight - 60

    g.setColor(if (backButtonHovered) new Color(0, 0, 100) else new Color(0, 0, 60))
    g.fillRect(backButtonX, backButtonY, backButtonWidth, backButtonHeight)
    g.setColor(if (backButtonHovered) highlightColor.brighter else highlightColor)
    g.setStroke(new BasicStroke(2))
    g.drawRect(backButtonX, backButtonY, backButtonWidth, backButtonHeight)
    g.setFont(new Font("Impact", Font.BOLD, 20))
    val backText = "VOLVER AL MENÚ"
    val backWidth = g.getFontMetrics.stringWidth(backText)

    g.setColor(Color.BLACK)
    g.drawString(backText, backButtonX + (backButtonWidth - backWidth) / 2 + 1, backButtonY + 27 + 1)
    g.setColor(if (backButtonHovered) highlightColor.brighter else Color.WHITE)
    g.drawString(backText, backButtonX + (backButtonWidth - backWidth) / 2, backButtonY + 27)
  }

  /**
   * Dibuja el control deslizante de volumen.
   * Incluye una barra de progreso, un control deslizante y
   * muestra el porcentaje actual del volumen.
   *
   * @param g Contexto gráfico donde dibujar
   */
  private def drawVolumeSlider(g: Graphics2D): Unit = {
    val sliderWidth = 200
    val sliderHeight = 10
    val sliderX = (size.width - sliderWidth) / 2
    val sliderY = size.height - 70

    volumeSliderRect = new Rectangle(sliderX, sliderY - 5, sliderWidth, sliderHeight + 10)

    g.setFont(new Font("Arial", Font.BOLD, 16))
    g.setColor(Color.WHITE)
    val volumeText = "VOLUMEN"
    val textWidth = g.getFontMetrics.stringWidth(volumeText)
    g.drawString(volumeText, sliderX + (sliderWidth - textWidth) / 2, sliderY - 15)

    g.setColor(new Color(0, 0, 60))
    g.fillRect(sliderX, sliderY, sliderWidth, sliderHeight)
    g.setColor(highlightColor)
    g.setStroke(new BasicStroke(1))
    g.drawRect(sliderX, sliderY, sliderWidth, sliderHeight)
    g.setColor(highlightColor)
    val volumeWidth = (sliderWidth * volumeHandlePosition / 100.0).toInt
    g.fillRect(sliderX, sliderY, volumeWidth, sliderHeight)
    val handleWidth = 10
    val handleHeight = 20
    val handleX = sliderX + volumeWidth - handleWidth / 2
    val handleY = sliderY - (handleHeight - sliderHeight) / 2
    g.setColor(Color.WHITE)
    g.fillRect(handleX, handleY, handleWidth, handleHeight)
    g.setColor(Color.GRAY)
    g.drawRect(handleX, handleY, handleWidth, handleHeight)

    // Mostrar porcentaje
    g.setFont(new Font("Arial", Font.PLAIN, 14))
    g.setColor(Color.WHITE)
    val percentText = s"$volumeHandlePosition%"
    g.drawString(percentText, sliderX + sliderWidth + 10, sliderY + sliderHeight / 2 + 5)
  }

  /**
   * Clase interna que representa un botón del menú.
   *
   * @param text  Texto que se mostrará en el botón
   * @param index Índice del botón en el menú
   */
  private class MenuButton(val text: String, val index: Int) {
    /** Posición X del botón */
    var x: Int = 0
    /** Posición Y del botón */
    var y: Int = 0
    /** Ancho del botón */
    var width: Int = 0
    /** Alto del botón */
    var height: Int = 0

    /**
     * Actualiza las dimensiones y posición del botón.
     *
     * @param newX      Nueva posición X
     * @param newY      Nueva posición Y
     * @param newWidth  Nuevo ancho
     * @param newHeight Nuevo alto
     */
    def update(newX: Int, newY: Int, newWidth: Int, newHeight: Int): Unit = {
      x = newX
      y = newY
      width = newWidth
      height = newHeight
    }

    /**
     * Verifica si un punto está dentro del área del botón.
     *
     * @param point Punto a verificar
     * @return true si el punto está dentro del botón
     */
    def contains(point: Point): Boolean = {
      point.x >= x && point.x <= (x + width) &&
        point.y >= y && point.y <= (y + height)
    }
  }
} 