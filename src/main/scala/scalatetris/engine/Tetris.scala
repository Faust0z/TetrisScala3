package scalatetris.engine

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import scalatetris.engine.EngineEvent.*
import scalatetris.ui.Display

/**
 * Objeto principal que implementa la lógica del juego Tetris usando el sistema de actores de Akka.
 *
 * Este objeto maneja:
 * - Los comandos del juego
 * - El ciclo de juego
 * - La interacción con el motor del juego
 * - La gestión del audio
 */
object Tetris {
  /** Trait sellado que define todos los comandos posibles en el juego */
  sealed trait Command

  /** Comando para continuar el juego desde pausa */
  case object Continue extends Command

  /** Comando para reiniciar el juego */
  case object Restart extends Command

  /** Comando para mover la pieza a la izquierda */
  case object Left extends Command

  /** Comando para mover la pieza a la derecha */
  case object Right extends Command

  /** Comando para mover la pieza hacia abajo */
  case object Down extends Command

  /** Comando para rotar la pieza en sentido antihorario */
  case object RotateLeft extends Command

  /** Comando para rotar la pieza en sentido horario */
  case object RotateRight extends Command

  /** Comando para pausar el juego */
  case object Pause extends Command

  /** Comando para actualizar el ciclo de juego */
  case object Tick extends Command

  /** Comando para indicar game over */
  case object GameOver extends Command

  /** Comando para guardar la pieza actual */
  case object Hold extends Command

  /** Comando para ubicar inmediatamente la pieza actual */
  case object HardDrop extends Command

  /**
   * Crea el comportamiento del actor Tetris.
   *
   * @param engine  Motor del juego que maneja la lógica
   * @param display Interfaz para mostrar el estado del juego
   * @return Comportamiento del actor configurado
   */
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

        case Restart  if (engine.isGameRunning == false)  =>
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
          while (engine.moveDown()) {}
          AudioManager.playSpeedSound()
          display.render(engine.stones, engine.points, engine.statistics, engine.isGameRunning)
          Behaviors.same


        case Tick =>
          if (engine.isGameRunning) {
            tickCounts += 6 // Bajan 1 bloque por segundo, ajustar según dificultad base
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