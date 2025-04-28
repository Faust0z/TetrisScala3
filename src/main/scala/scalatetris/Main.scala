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


object Main extends SimpleSwingApplication {
  private var highScore: Int = 0  // Récord personal guardado
  
  private def getScreenSize: AWTDimension = {
    Toolkit.getDefaultToolkit.getScreenSize
  }
  
  // Estados del juego
  private sealed trait GameState
  private case object MenuState extends GameState
  private case object PlayingState extends GameState
  
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
    
    // Función para iniciar el juego
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
      
      // Trait Display requerido por Tetris
      trait Display {
        def render(data: String): Unit
      }
      
      // Display que ahora no usa texto, pero se requiere para el actor
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
    
    // Función para mostrar el menú principal
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
  
  private def toggleFullScreen(frame: MainFrame, device: GraphicsDevice): Unit = {
    if (device.getFullScreenWindow == null) {
      device.setFullScreenWindow(frame.peer)
    } else {
      device.setFullScreenWindow(null)
    }
  }
  
  // Método para actualizar y guardar la puntuación más alta
  def updateHighScore(score: Int): Unit = {
    if (score > highScore) {
      highScore = score
      // Aquí podrías implementar la persistencia para guardar el récord en disco
    }
  }
  
  // Método para obtener la puntuación más alta
  def getHighScore: Int = highScore
}