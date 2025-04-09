package scalatetris

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import scalatetris.EngineEvent._
import scalatetris.UserInteraction._
import scalatetris.engine.GameEngine

object Tetris {
  // Define the messages the actor can receive
  sealed trait Command

  case object Continue extends Command

  case object Restart extends Command

  case object Left extends Command

  case object Right extends Command

  case object Down extends Command

  case object RotateLeft extends Command

  case object RotateRight extends Command

  case object Pause extends Command

  case object Tick extends Command

  def apply(engine: GameEngine, display: Display): Behavior[Command] =
    Behaviors.setup { _ =>
      var tickCounts = 0

      Behaviors.receiveMessage {
        case Continue =>
          engine.continue()
          display.render(engine.draw())
          Behaviors.same

        case Restart if !engine.isGameRunning =>
          engine.restart()
          Behaviors.same

        case Left if !engine.isGameRunning =>
          engine.backwardInTime()
          display.render(engine.draw())
          Behaviors.same

        case Right if !engine.isGameRunning =>
          engine.backIntoTheFuture()
          display.render(engine.draw())
          Behaviors.same

        case Left =>
          engine.moveLeft()
          display.render(engine.draw())
          Behaviors.same

        case Right =>
          engine.moveRight()
          display.render(engine.draw())
          Behaviors.same

        case Down =>
          engine.moveDown()
          display.render(engine.draw())
          Behaviors.same

        case RotateLeft =>
          engine.rotateLeft()
          display.render(engine.draw())
          Behaviors.same

        case RotateRight =>
          engine.rotateRight()
          display.render(engine.draw())
          Behaviors.same

        case Pause =>
          engine.pause()
          display.render(engine.draw())
          Behaviors.same

        case Tick =>
          tickCounts += 1
          if (tickCounts % 5 == 0) {
            engine.moveDown()
          }
          display.render(engine.draw())
          Behaviors.same
      }
    }
}
