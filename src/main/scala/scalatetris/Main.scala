package scalatetris

import akka.actor.{ActorSystem, Props}
import scalatetris.EngineEvent._
import scalatetris.UserInteraction._
import scalatetris.engine.GameEngine
import scalatetris.environment._

import java.awt.Font
import scala.concurrent.duration.Duration
import scala.swing.{Dimension, Frame, MainFrame, SimpleSwingApplication, TextArea}
import scala.swing.event.{Key, KeyPressed}

object Main extends SimpleSwingApplication {
  def top: Frame = {
    val area = new TextArea {
      font = new Font(Font.MONOSPACED, Font.PLAIN, 20)
      preferredSize = new Dimension(1000, 1000)
      editable = false
    }

    val frame = new MainFrame {
      title = "Scala Tetris"
      contents = area
      preferredSize = new Dimension(640, 768)
    }

    val engine = new GameEngine(Size(10, 20), RandomStoneFactory)
    val display = new SwingDisplay(area)
    val drawing = engine.draw()
    display.render(drawing)

    val system = ActorSystem()
    val tetris = {
      system.actorOf(Props(new Tetris(engine, display)), name = "tetris")
    }
    import system.dispatcher
    system.scheduler.schedule(
      Duration(500, "ms"),
      Duration(100, "ms"),
      tetris,
      Tick)

    listenTo(area.keys)
    reactions += {
      case key: KeyPressed => {
        key.key match {
          case Key.A =>
            tetris ! Left
          case Key.S =>
            tetris ! Down
          case Key.D =>
            tetris ! Right
          case Key.Q =>
            tetris ! RotateLeft
          case Key.E =>
            tetris ! RotateRight
          case Key.R =>
            tetris ! Restart
          case Key.P =>
            tetris ! Pause
          case Key.C =>
            tetris ! Continue
          case _ => ()
        }
      }
    }

    frame
  }
}
