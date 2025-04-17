package scalatetris.ui

import scalatetris.Main
import scalatetris.AudioManager

import scala.swing.*
import java.awt.{BasicStroke, Color, Font, FontMetrics, GradientPaint, Graphics2D}
import scala.swing.event.{Key, KeyPressed, MouseClicked, MouseMoved, MouseDragged}

class MainMenuPanel(onStartGame: () => Unit, onQuit: () => Unit) extends Panel {
  
  // Colores y diseño visual
  private val backgroundColor = new Color(0, 0, 30)  // Azul muy oscuro
  private val buttonColor = new Color(0, 0, 60)      // Azul para botones
  private val highlightColor = new Color(0, 191, 255) // Cyan brillante
  private val titleColor = new Color(255, 50, 50)     // Rojo brillante para el título
  
  private val buttons = List(
    new MenuButton("JUGAR", 0),
    new MenuButton("CONTROLES", 1),
    new MenuButton("SALIR", 2)
  )
  
  private var hoveredButton: Option[Int] = None
  private var showControls: Boolean = false
  private var backButtonHovered: Boolean = false  // Para detectar hover en el botón de volver al menú
  
  // Variables para el control de volumen
  private var volumeSliderRect: Rectangle = new Rectangle(0, 0, 0, 0)
  private var isDraggingVolume: Boolean = false
  private var volumeHandlePosition: Int = (AudioManager.getVolume * 100).toInt
  
  // Registrar eventos de mouse
  listenTo(mouse.clicks, mouse.moves, keys)
  reactions += {
    case e: MouseMoved =>
      updateHoveredButton(e.point)
      if (showControls) {
        // Verificar si el mouse está sobre el botón de volver
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
        volumeHandlePosition = (clampedX * 100 / volumeSliderRect.width).toInt
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
        
        // Si se hace clic en el botón de volver
        if (e.point.x >= backButtonX && e.point.x <= (backButtonX + backButtonWidth) &&
            e.point.y >= backButtonY && e.point.y <= (backButtonY + backButtonHeight)) {
          showControls = false
          repaint()
        }
      } else if (volumeSliderRect.contains(e.point)) {
        // Clic en el control de volumen
        val sliderX = e.point.x - volumeSliderRect.x
        volumeHandlePosition = (sliderX * 100 / volumeSliderRect.width).toInt
        AudioManager.setVolume(volumeHandlePosition / 100.0f)
        repaint()
      } else {
        hoveredButton match {
          case Some(0) => onStartGame() // JUGAR
          case Some(1) => 
            showControls = true
            repaint()
          case Some(2) => onQuit() // SALIR
          case _ => // Nada
        }
      }
      
      isDraggingVolume = false
  }
  
  private def updateHoveredButton(point: Point): Unit = {
    hoveredButton = buttons.find(_.contains(point)).map(_.index)
  }
  
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    
    // Configurar renderizado
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(java.awt.RenderingHints.KEY_TEXT_ANTIALIASING, java.awt.RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    
    // Fondo
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
  
  private def drawLogo(g: Graphics2D): Unit = {
    val title = "TETRIS"
    
    // Configuración de tamaño y posición del logo
    val titleSize = math.min(size.width / 8, 100)
    g.setFont(new Font("Impact", Font.BOLD, titleSize))
    
    val fm = g.getFontMetrics
    val titleWidth = fm.stringWidth(title)
    val x = (size.width - titleWidth) / 2
    val y = size.height / 4
    
    // Efecto de sombra para el título
    g.setColor(Color.BLACK)
    g.drawString(title, x + 4, y + 4)
    
    // Dibujar letras con efectos de bloques de Tetris
    val letters = title.toCharArray
    
    // Calcular el ancho total que ocuparán todas las letras
    val totalLetterWidth = letters.map(letter => fm.charWidth(letter)).sum
    
    // Calcular el espacio extra entre letras para distribuirlas uniformemente
    val extraSpace = (titleWidth - totalLetterWidth) / (letters.length - 1)
    
    var currentX = x
    
    for (i <- letters.indices) {
      val letterWidth = fm.charWidth(letters(i))
      
      // Colores diferentes para cada letra
      val letterColor = i match {
        case 0 => new Color(255, 0, 0)      // T: Rojo
        case 1 => new Color(255, 165, 0)    // E: Naranja
        case 2 => new Color(0, 255, 255)    // T: Cyan
        case 3 => new Color(0, 255, 0)      // R: Verde
        case 4 => new Color(255, 0, 255)    // I: Magenta
        case 5 => new Color(255, 255, 0)    // S: Amarillo
        case _ => Color.WHITE
      }
      
      // Corregir la posición de la letra I para alinearla correctamente
      val letterY = if (i == 4) y - 2 else y  // Ajustar un poco la posición de la I
      
      g.setColor(letterColor)
      g.drawString(letters(i).toString, currentX, letterY)
      
      // Añadir un borde de bloque a cada letra
      g.setStroke(new BasicStroke(2))
      g.drawRect(currentX - 2, y - titleSize + 10, letterWidth + 4, titleSize)
      
      // Avanzar a la siguiente posición con espaciado uniforme
      currentX += letterWidth + extraSpace
    }
    
    // Subtítulo
    g.setFont(new Font("Arial", Font.PLAIN, titleSize / 3))
    val subtitle = "SCALA EDITION"
    val subtitleWidth = g.getFontMetrics.stringWidth(subtitle)
    g.setColor(Color.WHITE)
    g.drawString(subtitle, (size.width - subtitleWidth) / 2, y + titleSize / 2)
  }
  
  private def drawButtons(g: Graphics2D): Unit = {
    val buttonWidth = math.min(size.width / 3, 300)
    val buttonHeight = 60
    val startY = size.height / 2
    val spacing = buttonHeight + 20
    
    buttons.foreach { button =>
      val y = startY + button.index * spacing
      button.update((size.width - buttonWidth) / 2, y, buttonWidth, buttonHeight)
      
      // Fondo del botón
      g.setColor(if (hoveredButton.contains(button.index)) buttonColor.brighter else buttonColor)
      g.fillRect(button.x, button.y, button.width, button.height)
      
      // Borde del botón
      g.setColor(if (hoveredButton.contains(button.index)) highlightColor else Color.GRAY)
      g.setStroke(new BasicStroke(3))
      g.drawRect(button.x, button.y, button.width, button.height)
      
      // Texto del botón
      val buttonFontSize = buttonHeight / 2
      g.setFont(new Font("Impact", Font.BOLD, buttonFontSize))
      val fm = g.getFontMetrics
      val textWidth = fm.stringWidth(button.text)
      val textX = button.x + (button.width - textWidth) / 2
      val textY = button.y + (button.height + buttonFontSize) / 2
      
      // Sombra del texto
      g.setColor(Color.BLACK)
      g.drawString(button.text, textX + 2, textY + 2)
      
      // Texto principal
      g.setColor(if (hoveredButton.contains(button.index)) highlightColor else Color.WHITE)
      g.drawString(button.text, textX, textY)
    }
  }
  
  private def drawControls(g: Graphics2D): Unit = {
    val panelWidth = math.min(size.width - 100, 600)
    val panelHeight = size.height - 200
    val panelX = (size.width - panelWidth) / 2
    val panelY = 100
    
    // Panel de fondo
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
    
    // Controles
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
    val keyWidth = 50  // Aumentado para acomodar teclas más largas como "ESC" y "F11"
    
    controls.foreach { case (key, action) =>
      // Fondo de la tecla
      g.setColor(new Color(40, 40, 40))
      g.fillRect(panelX + 50, yPos - fontSize, keyWidth, fontSize + 10)
      
      // Borde de la tecla
      g.setColor(new Color(100, 100, 100))
      g.setStroke(new BasicStroke(1))
      g.drawRect(panelX + 50, yPos - fontSize, keyWidth, fontSize + 10)
      
      // Texto de la tecla
      g.setColor(Color.WHITE)
      g.drawString(key, panelX + 50 + (keyWidth - g.getFontMetrics.stringWidth(key)) / 2, yPos)
      
      // Acción
      g.setColor(Color.LIGHT_GRAY)
      g.drawString(action, panelX + 50 + keyWidth + 20, yPos)
      
      yPos += spacing
    }
    
    // Botón para volver al menú en lugar de solo texto
    val backButtonWidth = 200
    val backButtonHeight = 40
    val backButtonX = panelX + (panelWidth - backButtonWidth) / 2
    val backButtonY = panelY + panelHeight - 60
    
    // Fondo del botón - ahora con efecto hover
    g.setColor(if (backButtonHovered) new Color(0, 0, 100) else new Color(0, 0, 60))
    g.fillRect(backButtonX, backButtonY, backButtonWidth, backButtonHeight)
    
    // Borde del botón - ahora con efecto hover
    g.setColor(if (backButtonHovered) highlightColor.brighter else highlightColor)
    g.setStroke(new BasicStroke(2))
    g.drawRect(backButtonX, backButtonY, backButtonWidth, backButtonHeight)
    
    // Texto del botón
    g.setFont(new Font("Impact", Font.BOLD, 20))
    val backText = "VOLVER AL MENÚ"
    val backWidth = g.getFontMetrics.stringWidth(backText)
    
    g.setColor(Color.BLACK)
    g.drawString(backText, backButtonX + (backButtonWidth - backWidth) / 2 + 1, backButtonY + 27 + 1)
    g.setColor(if (backButtonHovered) highlightColor.brighter else Color.WHITE)
    g.drawString(backText, backButtonX + (backButtonWidth - backWidth) / 2, backButtonY + 27)
  }
  
  private def drawVolumeSlider(g: Graphics2D): Unit = {
    val sliderWidth = 200
    val sliderHeight = 10
    val sliderX = (size.width - sliderWidth) / 2
    val sliderY = size.height - 70
    
    // Actualizar rectángulo del slider para detección de eventos
    volumeSliderRect = new Rectangle(sliderX, sliderY - 5, sliderWidth, sliderHeight + 10)
    
    // Dibujar etiqueta "VOLUMEN"
    g.setFont(new Font("Arial", Font.BOLD, 16))
    g.setColor(Color.WHITE)
    val volumeText = "VOLUMEN"
    val textWidth = g.getFontMetrics.stringWidth(volumeText)
    g.drawString(volumeText, sliderX + (sliderWidth - textWidth) / 2, sliderY - 15)
    
    // Dibujar fondo del slider
    g.setColor(new Color(0, 0, 60))
    g.fillRect(sliderX, sliderY, sliderWidth, sliderHeight)
    
    // Dibujar borde del slider
    g.setColor(highlightColor)
    g.setStroke(new BasicStroke(1))
    g.drawRect(sliderX, sliderY, sliderWidth, sliderHeight)
    
    // Dibujar nivel de volumen
    g.setColor(highlightColor)
    val volumeWidth = (sliderWidth * volumeHandlePosition / 100.0).toInt
    g.fillRect(sliderX, sliderY, volumeWidth, sliderHeight)
    
    // Dibujar control deslizante
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
    val percentText = s"${volumeHandlePosition}%"
    g.drawString(percentText, sliderX + sliderWidth + 10, sliderY + sliderHeight / 2 + 5)
  }
  
  private class MenuButton(val text: String, val index: Int) {
    var x: Int = 0
    var y: Int = 0
    var width: Int = 0
    var height: Int = 0
    
    def update(newX: Int, newY: Int, newWidth: Int, newHeight: Int): Unit = {
      x = newX
      y = newY
      width = newWidth
      height = newHeight
    }
    
    def contains(point: Point): Boolean = {
      point.x >= x && point.x <= (x + width) &&
      point.y >= y && point.y <= (y + height)
    }
  }
} 