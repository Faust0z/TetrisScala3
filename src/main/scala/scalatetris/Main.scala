package scalatetris

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import scalatetris.engine.GameEngine
import scalatetris.environment._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.swing._
import scala.swing.event.{Key, KeyPressed}
import java.awt.{GraphicsDevice, GraphicsEnvironment, Toolkit, Dimension => AWTDimension}
import java.awt.event.{ComponentAdapter, ComponentEvent}
import scalatetris.ui.MainMenuPanel

/**
 * Objeto principal que inicia y gestiona la aplicación Tetris.
 * 
 * Este objeto extiende SimpleSwingApplication, proporcionando la estructura básica
 * para una aplicación gráfica en Scala. Gestiona la alternancia entre el menú principal
 * y la pantalla de juego, controla la música, maneja eventos de teclado y pantalla completa.
 */
object Main extends SimpleSwingApplication {
  /** Récord personal guardado entre sesiones de juego */
  private var highScore: Int = 0
  
  /**
   * Obtiene el tamaño de la pantalla actual del sistema.
   * 
   * @return Dimensión de la pantalla actual
   */
  private def getScreenSize: AWTDimension = {
    Toolkit.getDefaultToolkit.getScreenSize
  }
  
  /** 
   * Estados del juego representados como tipos algebraicos.
   * Determinan si se muestra el menú o se está jugando.
   */
  private sealed trait GameState
  /** Estado cuando se muestra el menú principal */
  private case object MenuState extends GameState
  /** Estado cuando se está jugando activamente */
  private case object PlayingState extends GameState
  
  /**
   * Crea y configura la ventana principal de la aplicación.
   * 
   * @return Frame que contiene la interfaz principal
   */
  def top: Frame = {
    // Cargar todos los sonidos
    AudioManager.loadAllSounds()
    
    val screenSize = getScreenSize
    val frameWidth = (screenSize.width * 0.8).toInt
    val frameHeight = (screenSize.height * 0.8).toInt
    
    // Crear un CardLayout para alternar entre menú y juego
    val cardPanel = new Panel {
      preferredSize = new Dimension(frameWidth, frameHeight)
    }
    
    // Frame principal
    val frame = new MainFrame {
      title = "Scala Tetris"
      contents = cardPanel
      
      // Configurar tamaño inicial al 80% de la pantalla
      preferredSize = new Dimension(frameWidth, frameHeight)
      
      // Centrar en la pantalla
      location = new java.awt.Point((screenSize.width - frameWidth) / 2, (screenSize.height - frameHeight) / 2)
      
      // Permitir maximizar
      resizable = true
      peer.setExtendedState(peer.getExtendedState | java.awt.Frame.MAXIMIZED_BOTH)
    }
    
    // Configurar pantalla completa
    val graphicsDevice: GraphicsDevice = GraphicsEnvironment
      .getLocalGraphicsEnvironment
      .getDefaultScreenDevice
      
    // Referencia al sistema de actores y al panel del juego
    var tetrisActorSystem: Option[ActorSystem[Tetris.Command]] = None
    var gamePanel: Option[TetrisPanel] = None
    
    /**
     * Inicia el juego creando el sistema de actores, panel de juego y configurando eventos.
     * 
     * Esta función configura el motor del juego, crea el sistema de actores para procesar
     * comandos, establece las reacciones a eventos de teclado y actualiza la interfaz.
     */
    def startGame(): Unit = {
      // Calcular el tamaño de bloque basado en la resolución de la pantalla
      val blockSize = math.min(screenSize.width / 30, screenSize.height / 30)
      
      val engine = new GameEngine(Size(10, 20), RandomStoneFactory)
      val panel = new TetrisPanel(engine, blockSize)
      
      // Escuchar cambios de tamaño para reajustar el panel
      panel.peer.addComponentListener(new ComponentAdapter {
        override def componentResized(e: ComponentEvent): Unit = {
          panel.resizeUI()
          panel.repaint()
        }
      })
      
      // Configurar para poder escuchar teclas
      panel.focusable = true
      panel.requestFocus()
      
      /**
       * Trait Display requerido por Tetris para renderizar el estado del juego.
       */
      trait Display {
        /**
         * Renderiza el estado actual del juego.
         * 
         * @param data Información del estado que se renderizará
         */
        def render(data: String): Unit
      }
      
      /**
       * Implementación de Display que no realiza acciones visuales.
       * Se utiliza porque el renderizado gráfico se maneja a través de TetrisPanel.
       */
      class DummyDisplay extends Display {
        override def render(data: String): Unit = ()
      }
      
      val dummyDisplay = new DummyDisplay
      val system: ActorSystem[Tetris.Command] = ActorSystem(Tetris(engine, new scalatetris.DummyDisplay), "ScalaTetrisSystem")
      val tetris = system.systemActorOf(Tetris(engine, new scalatetris.DummyDisplay), "tetris")
      
      implicit val timeout: Timeout = 3.seconds
      implicit val ec: ExecutionContext = system.executionContext
      
      // Iniciar el timer para el juego
      val tickTask = system.scheduler.scheduleAtFixedRate(500.millis, 100.millis) { () =>
        system ! Tetris.Tick
        panel.repaint()
      }
      
      // Configurar reacciones a teclas
      frame.listenTo(panel.keys)
      frame.reactions += {
        case key: KeyPressed =>
          key.key match {
            case Key.A => tetris ! Tetris.Left
            case Key.S => tetris ! Tetris.Down
            case Key.D => tetris ! Tetris.Right
            case Key.Q => tetris ! Tetris.RotateLeft
            case Key.E => tetris ! Tetris.RotateRight
            case Key.R => tetris ! Tetris.Restart
            case Key.P => tetris ! Tetris.Pause
            case Key.C => tetris ! Tetris.Continue
            case Key.H => tetris ! Tetris.Hold
            case Key.M => // Tecla M para volver al menú
              tetris ! Tetris.Pause
              AudioManager.stopMusic()
              showMainMenu()
            case Key.F11 => toggleFullScreen(frame, graphicsDevice)
            case Key.Escape => 
              if (graphicsDevice.getFullScreenWindow != null) {
                toggleFullScreen(frame, graphicsDevice)
              } else {
                // Volver al menú principal
                tetris ! Tetris.Pause
                AudioManager.stopMusic()
                showMainMenu()
              }
            case _ => ()
          }
      }
      
      // Guardar referencias
      tetrisActorSystem = Some(system)
      gamePanel = Some(panel)
      
      // Cambiar a la pantalla de juego
      cardPanel.peer.setLayout(new java.awt.BorderLayout())
      cardPanel.peer.removeAll()
      cardPanel.peer.add(panel.peer, java.awt.BorderLayout.CENTER)
      cardPanel.revalidate()
      panel.requestFocusInWindow()
      
      // Iniciar la música del juego
      AudioManager.playMusic()
    }
    
    /**
     * Muestra el menú principal, deteniendo cualquier juego en progreso.
     * 
     * Esta función termina el sistema de actores del juego si existe,
     * y configura el panel para mostrar el menú principal con botones
     * para iniciar el juego o salir.
     */
    def showMainMenu(): Unit = {
      // Detener el sistema de actores si existe
      tetrisActorSystem.foreach(_.terminate())
      tetrisActorSystem = None
      
      // Reproducir música del menú
      AudioManager.stopMusic()
      AudioManager.playMusic()
      
      // Crear el panel del menú
      val menuPanel = new MainMenuPanel(
        // Función para iniciar el juego
        onStartGame = () => {
          startGame()
        },
        // Función para salir
        onQuit = () => {
          System.exit(0)
        }
      )
      
      // Cambiar a la pantalla de menú
      cardPanel.peer.setLayout(new java.awt.BorderLayout())
      cardPanel.peer.removeAll()
      cardPanel.peer.add(menuPanel.peer, java.awt.BorderLayout.CENTER)
      cardPanel.revalidate()
      menuPanel.requestFocusInWindow()
    }
    
    // Mostrar el menú principal al iniciar
    showMainMenu()
    
    frame
  }
  
  /**
   * Alterna entre modo pantalla completa y ventana normal.
   * 
   * @param frame El marco principal que se mostrará en pantalla completa
   * @param device El dispositivo gráfico que gestionará la pantalla completa
   */
  private def toggleFullScreen(frame: MainFrame, device: GraphicsDevice): Unit = {
    if (device.getFullScreenWindow == null) {
      device.setFullScreenWindow(frame.peer)
    } else {
      device.setFullScreenWindow(null)
    }
  }
  
  /**
   * Actualiza y guarda la puntuación más alta si la puntuación actual la supera.
   * 
   * @param score La puntuación actual para comparar con el récord
   */
  def updateHighScore(score: Int): Unit = {
    if (score > highScore) {
      highScore = score
    }
  }
  
  /**
   * Devuelve la puntuación más alta registrada.
   * 
   * @return El valor del récord personal
   */
  def getHighScore: Int = highScore
}