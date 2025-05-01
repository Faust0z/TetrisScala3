package scalatetris.engine

/** 
 * Enumeración que define los posibles eventos de interacción del usuario.
 * 
 * Estos eventos representan las acciones que el usuario puede realizar
 * durante el juego, como mover piezas, rotarlas o controlar el estado del juego.
 */
object UserInteraction extends Enumeration {
  type UserInteraction = Value
  val Left, Right, Down, RotateLeft, RotateRight, Restart, Pause, Continue  = Value
}

/** 
 * Enumeración que define los eventos del motor del juego.
 * 
 * Estos eventos son generados por el motor del juego para controlar
 * su funcionamiento interno, como el ciclo de actualización.
 */
object EngineEvent extends Enumeration {
  type EngineEvent = Value
  /** Evento de actualización del ciclo de juego */
  val Tick: Value = Value
}