package scalatetris

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import scalatetris.EngineEvent._
import scalatetris.engine.GameEngine

object Tetris {
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
  case object GameOver extends Command
  case object Hold extends Command

  case object HardDrop extends Command


  def apply(engine: GameEngine, display: Display): Behavior[Command] =
    Behaviors.setup { _ =>
      var tickCounts = 0

      Behaviors.receiveMessage {
        case Continue =>
          if (!engine.IsRunning && engine.boardIsRunning) {
            engine.continue()
            AudioManager.resumeMusic()
          }
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case Restart =>
          engine.restart()
          AudioManager.stopMusic()
          AudioManager.playMusic()
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case Left if engine.isGameRunning =>
          engine.moveLeft()
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case Right if engine.isGameRunning =>
          engine.moveRight()
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case Down if engine.isGameRunning =>
          AudioManager.playSpeedSound()
          engine.moveDown()
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case RotateLeft if engine.isGameRunning =>
          engine.rotateLeft()
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case RotateRight if engine.isGameRunning =>
          engine.rotateRight()
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case Hold if engine.isGameRunning =>
          engine.holdCurrentStone()
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case Pause if engine.isGameRunning =>
          engine.pause()
          AudioManager.pauseMusic()
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case HardDrop if engine.isGameRunning =>
          while (engine.moveDown()) {} // Mover hacia abajo hasta que no pueda más
          AudioManager.playSpeedSound() // Opcional: sonido de hard drop si querés
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same


        case Tick =>
          if (engine.isGameRunning) {
            tickCounts += 8 // Bajan 1 bloque por segundo, ajustar según dificultad base
            val speedFactor = math.max(engine.getSpeedFactor, 1)

            if (tickCounts >= speedFactor) {
              engine.moveDown()
              tickCounts = 0
            }
          }
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same

        case _ =>
          Behaviors.unhandled
      }
    }
}