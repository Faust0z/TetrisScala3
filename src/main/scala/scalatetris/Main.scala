package scalatetris

import akka.actor.typed.ActorSystem
import scalatetris.UserInteraction._
import scalatetris.engine.GameEngine
import scalatetris.environment._

import java.awt.Font
import scala.swing.{Dimension, Frame, MainFrame, SimpleSwingApplication, TextArea}
import scala.swing.event.{Key, KeyPressed}

import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

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

    val system: ActorSystem[Tetris.Command] = ActorSystem(Tetris(engine, display), "ScalaTetrisSystem")
    val tetris = system.systemActorOf(Tetris(engine, display), "tetris")


    implicit val timeout: Timeout = 3.seconds
    implicit val ec: ExecutionContext = system.executionContext
    system.scheduler.scheduleAtFixedRate(500.millis, 100.millis) {
      () => system ! Tetris.Tick
    }

    listenTo(area.keys)
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
