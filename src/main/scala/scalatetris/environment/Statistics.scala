package scalatetris.environment

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

/** 
 * Clase que maneja las estadísticas del juego.
 * 
 * Esta clase inmutable mantiene el registro de:
 * - Tiempo de juego
 * - Filas completadas
 * - Puntuación actual y pendiente
 * - Tiempo en pausa
 * 
 * @param startTime Momento de inicio del juego
 * @param rowsCompleted Número de filas completadas
 * @param score Puntuación actual
 * @param pendingScore Puntuación pendiente de aplicar
 */
case class Statistics(startTime: Date, rowsCompleted: Int, score: Int, pendingScore: Int) {

  /** 
   * Incrementa el contador de filas completadas y añade puntos si corresponde.
   * 
   * @param numberOfRows Número de filas completadas en esta jugada
   * @return Nueva instancia con las estadísticas actualizadas
   */
  def anotherRowHasBeenCompleted(numberOfRows: Int): Statistics = {
    // Primero incrementamos el contador de filas
    val withIncrementedRows = copy(rowsCompleted = rowsCompleted + numberOfRows)

    // Después agregamos puntos por las líneas completadas si hay alguna
    if (numberOfRows > 0)
      withIncrementedRows.addLinePoints(numberOfRows)
    else
      withIncrementedRows
  }

  /** 
   * Añade puntos por tiempo de juego.
   * 
   * @return Nueva instancia con la puntuación incrementada
   */
  def addTimePoints(): Statistics =
    copy(score = score + 3)

  /** 
   * Añade puntos por líneas completadas a la puntuación pendiente.
   * Los puntos se calculan usando una fórmula que premia completar
   * múltiples líneas a la vez.
   * 
   * @param numberOfRows Número de filas completadas
   * @return Nueva instancia con la puntuación pendiente actualizada
   */
  private def addLinePoints(numberOfRows: Int): Statistics =
    copy(pendingScore = pendingScore + (500 * numberOfRows * (numberOfRows + 3) / 2))

  /** 
   * Aplica una porción de los puntos pendientes a la puntuación actual.
   * Este método permite una animación suave del incremento de puntos.
   * 
   * @return Nueva instancia con puntos pendientes parcialmente aplicados
   */
  def applyPendingPoints(): Statistics = {
    val increment = math.max(100, pendingScore / 4)
    if (pendingScore > 0) copy(score = score + increment, pendingScore = pendingScore - increment) else this
  }

  // Variable para almacenar el tiempo en pausa para esta instancia
  private var pausedTime: Long = 0
  
  /** 
   * Actualiza el tiempo total en pausa.
   * 
   * @param newPausedTime Nuevo tiempo total en pausa en milisegundos
   * @return Esta instancia con el tiempo en pausa actualizado
   */
  def withPausedTime(newPausedTime: Long): Statistics = {
    // Actualizar el tiempo en pausa para esta instancia
    pausedTime = newPausedTime
    this
  }

  /** 
   * Genera una representación en texto de las estadísticas actuales.
   * 
   * Incluye:
   * - Número de filas completadas
   * - Tiempo de juego (descontando pausas)
   * - Puntuación actual y pendiente
   * 
   * @return String con las estadísticas formateadas
   */
  def draw(): String = {
    val now: Date = Calendar.getInstance().getTime
    val duration: Long = now.getTime - startTime.getTime - pausedTime
    val format = new SimpleDateFormat("mm:ss")
    var displayPendingScore = ""
    if (pendingScore > 0) displayPendingScore = f" +$pendingScore" else ""

    // Asegurarnos de que la duración no sea negativa
    val adjustedDuration = math.max(0, duration)

    // Formatear el tiempo ajustado
    val formatDate = new Date(adjustedDuration)

    s"Filas: $rowsCompleted\nTiempo: ${format.format(formatDate)}\nPuntos: $score$displayPendingScore"
  }
}
