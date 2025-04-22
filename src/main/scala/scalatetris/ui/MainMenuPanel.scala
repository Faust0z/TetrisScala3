package scalatetris.ui

import scalatetris.Main
import scalatetris.AudioManager

import scala.swing.*
import java.awt.{BasicStroke, Color, Font, FontMetrics, GradientPaint, Graphics2D}
import scala.swing.event.{Key, KeyPressed, MouseClicked, MouseMoved, MouseDragged}

/** 
 * Panel that implements the main menu of the game.
 * 
 * This panel handles:
 * - The main menu interface
 * - Interactive buttons
 * - The controls screen
 * - Volume control
 * - Visual effects and animations
 * 
 * @param onStartGame Function to execute when the game starts
 * @param onQuit Function to execute when the game quits
 */
class MainMenuPanel(onStartGame: () => Unit, onQuit: () => Unit) extends Panel {
  
  /** Colors of the menu theme */
  private val backgroundColor = new Color(0, 0, 30)  // Dark blue
  private val buttonColor = new Color(0, 0, 60)      // Blue for buttons
  private val highlightColor = new Color(0, 191, 255) // Bright cyan
  private val titleColor = new Color(255, 50, 50)     // Bright red for the title
  
  /** List of main menu buttons */
  private val buttons = List(
    new MenuButton("JUGAR", 0),
    new MenuButton("CONTROLES", 1),
    new MenuButton("SALIR", 2)
  )
  
  /** Index of the button under the cursor, if any */
  private var hoveredButton: Option[Int] = None
  /** Indicates if the controls screen is being shown */
  private var showControls: Boolean = false
  /** Indicates if the cursor is over the back to menu button */
  private var backButtonHovered: Boolean = false
  
  /** Rectangle that defines the volume control area */
  private var volumeSliderRect: Rectangle = new Rectangle(0, 0, 0, 0)
  /** Indicates if the volume control is being dragged */
  private var isDraggingVolume: Boolean = false
  /** Current position of the volume control (0-100) */
  private var volumeHandlePosition: Int = (AudioManager.getVolume * 100).toInt
  
  // Register mouse and keyboard events
  listenTo(mouse.clicks, mouse.moves, keys)
  
  /** 
   * Event handler for the panel.
   * Processes mouse and keyboard events for:
   * - Detecting hover over buttons
   * - Handling button clicks
   * - Controlling the volume
   * - Handling the ESC key
   */
  reactions += {
    case e: MouseMoved =>
      updateHoveredButton(e.point)
      if (showControls) {
        // Check if the mouse is over the back button
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
        
        // If the back button is clicked
        if (e.point.x >= backButtonX && e.point.x <= (backButtonX + backButtonWidth) &&
            e.point.y >= backButtonY && e.point.y <= (backButtonY + backButtonHeight)) {
          showControls = false
          repaint()
        }
      } else if (volumeSliderRect.contains(e.point)) {
        // Click on the volume control
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
  
  /** 
   * Updates the button under the cursor.
   * 
   * @param point Current cursor position
   */
  private def updateHoveredButton(point: Point): Unit = {
    hoveredButton = buttons.find(_.contains(point)).map(_.index)
  }
  
  /** 
   * Draws all components of the panel.
   * 
   * @param g Graphics context where to draw
   */
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    
    // Set rendering hints
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(java.awt.RenderingHints.KEY_TEXT_ANTIALIASING, java.awt.RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    
    // Background
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
   * Draws the game logo with visual effects.
   * Includes shadow effects, neon colors and Tetris-style blocks.
   * 
   * @param g Graphics context where to draw
   */
  private def drawLogo(g: Graphics2D): Unit = {
    val title = "TETRIS"
    
    // Set logo size and position
    val titleSize = math.min(size.width / 8, 100)
    g.setFont(new Font("Impact", Font.BOLD, titleSize))
    
    val fm = g.getFontMetrics
    val titleWidth = fm.stringWidth(title)
    val x = (size.width - titleWidth) / 2
    val y = size.height / 4
    
    // Shadow effect for the title
    g.setColor(Color.BLACK)
    g.drawString(title, x + 4, y + 4)
    
    // Draw letters with Tetris block effects
    val letters = title.toCharArray
    
    // Calculate the total width that all letters will occupy
    val totalLetterWidth = letters.map(letter => fm.charWidth(letter)).sum
    
    // Calculate the extra space between letters to distribute them evenly
    val extraSpace = (titleWidth - totalLetterWidth) / (letters.length - 1)
    
    var currentX = x
    
    for (i <- letters.indices) {
      val letterWidth = fm.charWidth(letters(i))
      
      // Different colors for each letter
      val letterColor = i match {
        case 0 => new Color(255, 0, 0)      // T: Red
        case 1 => new Color(255, 165, 0)    // E: Orange
        case 2 => new Color(0, 255, 255)    // T: Cyan
        case 3 => new Color(0, 255, 0)      // R: Green
        case 4 => new Color(255, 0, 255)    // I: Magenta
        case 5 => new Color(255, 255, 0)    // S: Yellow
        case _ => Color.WHITE
      }
      
      // Correct the position of the I letter to align it correctly
      val letterY = if (i == 4) y - 2 else y  // Slightly adjust the position of the I
      
      g.setColor(letterColor)
      g.drawString(letters(i).toString, currentX, letterY)
      
      // Add a block border to each letter
      g.setStroke(new BasicStroke(2))
      g.drawRect(currentX - 2, y - titleSize + 10, letterWidth + 4, titleSize)
      
      // Advance to the next position with uniform spacing
      currentX += letterWidth + extraSpace
    }
    
    // Subtitle
    g.setFont(new Font("Arial", Font.PLAIN, titleSize / 3))
    val subtitle = "SCALA EDITION"
    val subtitleWidth = g.getFontMetrics.stringWidth(subtitle)
    g.setColor(Color.WHITE)
    g.drawString(subtitle, (size.width - subtitleWidth) / 2, y + titleSize / 2)
  }
  
  /** 
   * Draws the main menu buttons with visual effects.
   * Includes hover effects, shadows and bright borders.
   * 
   * @param g Graphics context where to draw
   */
  private def drawButtons(g: Graphics2D): Unit = {
    val buttonWidth = math.min(size.width / 3, 300)
    val buttonHeight = 60
    val startY = size.height / 2
    val spacing = buttonHeight + 20
    
    buttons.foreach { button =>
      val y = startY + button.index * spacing
      button.update((size.width - buttonWidth) / 2, y, buttonWidth, buttonHeight)
      
      // Button background
      g.setColor(if (hoveredButton.contains(button.index)) buttonColor.brighter else buttonColor)
      g.fillRect(button.x, button.y, button.width, button.height)
      
      // Button border
      g.setColor(if (hoveredButton.contains(button.index)) highlightColor else Color.GRAY)
      g.setStroke(new BasicStroke(3))
      g.drawRect(button.x, button.y, button.width, button.height)
      
      // Button text
      val buttonFontSize = buttonHeight / 2
      g.setFont(new Font("Impact", Font.BOLD, buttonFontSize))
      val fm = g.getFontMetrics
      val textWidth = fm.stringWidth(button.text)
      val textX = button.x + (button.width - textWidth) / 2
      val textY = button.y + (button.height + buttonFontSize) / 2
      
      // Text shadow
      g.setColor(Color.BLACK)
      g.drawString(button.text, textX + 2, textY + 2)
      
      // Main text
      g.setColor(if (hoveredButton.contains(button.index)) highlightColor else Color.WHITE)
      g.drawString(button.text, textX, textY)
    }
  }
  
  /** 
   * Draws the game controls screen.
   * Displays a list of all keys and their functions,
   * with visual effects and a back to menu button.
   * 
   * @param g Graphics context where to draw
   */
  private def drawControls(g: Graphics2D): Unit = {
    val panelWidth = math.min(size.width - 100, 600)
    val panelHeight = size.height - 200
    val panelX = (size.width - panelWidth) / 2
    val panelY = 100
    
    // Background panel
    g.setColor(new Color(0, 0, 40, 220))
    g.fillRect(panelX, panelY, panelWidth, panelHeight)
    
    g.setColor(highlightColor)
    g.setStroke(new BasicStroke(3))
    g.drawRect(panelX, panelY, panelWidth, panelHeight)
    
    // Title
    val titleFontSize = 40
    g.setFont(new Font("Impact", Font.BOLD, titleFontSize))
    val title = "CONTROLES"
    val fm = g.getFontMetrics
    val titleWidth = fm.stringWidth(title)
    
    g.setColor(Color.BLACK)
    g.drawString(title, panelX + (panelWidth - titleWidth) / 2 + 2, panelY + 50 + 2)
    g.setColor(highlightColor)
    g.drawString(title, panelX + (panelWidth - titleWidth) / 2, panelY + 50)
    
    // Controls
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
    val keyWidth = 50  // Increased to accommodate longer keys like "ESC" and "F11"
    
    controls.foreach { case (key, action) =>
      // Key background
      g.setColor(new Color(40, 40, 40))
      g.fillRect(panelX + 50, yPos - fontSize, keyWidth, fontSize + 10)
      
      // Key border
      g.setColor(new Color(100, 100, 100))
      g.setStroke(new BasicStroke(1))
      g.drawRect(panelX + 50, yPos - fontSize, keyWidth, fontSize + 10)
      
      // Key text
      g.setColor(Color.WHITE)
      g.drawString(key, panelX + 50 + (keyWidth - g.getFontMetrics.stringWidth(key)) / 2, yPos)
      
      // Action
      g.setColor(Color.LIGHT_GRAY)
      g.drawString(action, panelX + 50 + keyWidth + 20, yPos)
      
      yPos += spacing
    }
    
    // Back to menu button instead of just text
    val backButtonWidth = 200
    val backButtonHeight = 40
    val backButtonX = panelX + (panelWidth - backButtonWidth) / 2
    val backButtonY = panelY + panelHeight - 60
    
    // Button background - now with hover effect
    g.setColor(if (backButtonHovered) new Color(0, 0, 100) else new Color(0, 0, 60))
    g.fillRect(backButtonX, backButtonY, backButtonWidth, backButtonHeight)
    
    // Button border - now with hover effect
    g.setColor(if (backButtonHovered) highlightColor.brighter else highlightColor)
    g.setStroke(new BasicStroke(2))
    g.drawRect(backButtonX, backButtonY, backButtonWidth, backButtonHeight)
    
    // Button text
    g.setFont(new Font("Impact", Font.BOLD, 20))
    val backText = "VOLVER AL MENÚ"
    val backWidth = g.getFontMetrics.stringWidth(backText)
    
    g.setColor(Color.BLACK)
    g.drawString(backText, backButtonX + (backButtonWidth - backWidth) / 2 + 1, backButtonY + 27 + 1)
    g.setColor(if (backButtonHovered) highlightColor.brighter else Color.WHITE)
    g.drawString(backText, backButtonX + (backButtonWidth - backWidth) / 2, backButtonY + 27)
  }
  
  /** 
   * Draws the volume slider.
   * Includes a progress bar, a slider control and
   * displays the current volume percentage.
   * 
   * @param g Graphics context where to draw
   */
  private def drawVolumeSlider(g: Graphics2D): Unit = {
    val sliderWidth = 200
    val sliderHeight = 10
    val sliderX = (size.width - sliderWidth) / 2
    val sliderY = size.height - 70
    
    // Update slider rectangle for event detection
    volumeSliderRect = new Rectangle(sliderX, sliderY - 5, sliderWidth, sliderHeight + 10)
    
    // Draw "VOLUME" label
    g.setFont(new Font("Arial", Font.BOLD, 16))
    g.setColor(Color.WHITE)
    val volumeText = "VOLUMEN"
    val textWidth = g.getFontMetrics.stringWidth(volumeText)
    g.drawString(volumeText, sliderX + (sliderWidth - textWidth) / 2, sliderY - 15)
    
    // Draw slider background
    g.setColor(new Color(0, 0, 60))
    g.fillRect(sliderX, sliderY, sliderWidth, sliderHeight)
    
    // Draw slider border
    g.setColor(highlightColor)
    g.setStroke(new BasicStroke(1))
    g.drawRect(sliderX, sliderY, sliderWidth, sliderHeight)
    
    // Draw volume level
    g.setColor(highlightColor)
    val volumeWidth = (sliderWidth * volumeHandlePosition / 100.0).toInt
    g.fillRect(sliderX, sliderY, volumeWidth, sliderHeight)
    
    // Draw slider control
    val handleWidth = 10
    val handleHeight = 20
    val handleX = sliderX + volumeWidth - handleWidth / 2
    val handleY = sliderY - (handleHeight - sliderHeight) / 2
    
    g.setColor(Color.WHITE)
    g.fillRect(handleX, handleY, handleWidth, handleHeight)
    g.setColor(Color.GRAY)
    g.drawRect(handleX, handleY, handleWidth, handleHeight)
    
    // Display percentage
    g.setFont(new Font("Arial", Font.PLAIN, 14))
    g.setColor(Color.WHITE)
    val percentText = s"${volumeHandlePosition}%"
    g.drawString(percentText, sliderX + sliderWidth + 10, sliderY + sliderHeight / 2 + 5)
  }
  
  /** 
   * Inner class that represents a menu button.
   * 
   * @param text Text to display on the button
   * @param index Index of the button in the menu
   */
  private class MenuButton(val text: String, val index: Int) {
    /** X position of the button */
    var x: Int = 0
    /** Y position of the button */
    var y: Int = 0
    /** Width of the button */
    var width: Int = 0
    /** Height of the button */
    var height: Int = 0
    
    /** 
     * Updates the dimensions and position of the button.
     * 
     * @param newX New X position
     * @param newY New Y position
     * @param newWidth New width
     * @param newHeight New height
     */
    def update(newX: Int, newY: Int, newWidth: Int, newHeight: Int): Unit = {
      x = newX
      y = newY
      width = newWidth
      height = newHeight
    }
    
    /** 
     * Checks if a point is inside the button area.
     * 
     * @param point Point to check
     * @return true if the point is inside the button
     */
    def contains(point: Point): Boolean = {
      point.x >= x && point.x <= (x + width) &&
      point.y >= y && point.y <= (y + height)
    }
  }
}