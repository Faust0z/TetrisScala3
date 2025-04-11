package scalatetris

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import scalatetris.engine.GameEngine
import scalatetris.environment._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.swing._
import scala.swing.event.{Key, KeyPressed}


object Main extends SimpleSwingApplication {
  def top: Frame = {
    val engine = new GameEngine(Size(10, 20), RandomStoneFactory)
    val panel = new TetrisPanel(engine)

    val frame = new MainFrame {
      title = "Scala Tetris"
      contents = panel
      preferredSize = new Dimension(640, 768)
    }

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
    system.scheduler.scheduleAtFixedRate(500.millis, 100.millis) { () =>
      system ! Tetris.Tick
      panel.repaint()
    }

    listenTo(panel.keys)
    reactions += {
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
          case _ => ()
        }
    }

    frame
  }
}