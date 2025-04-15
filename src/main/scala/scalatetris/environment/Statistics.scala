package scalatetris.environment

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

case class Statistics(startTime: Date, rowsCompleted: Int, score: Int, pendingScore: Int) {

  def anotherRowHasBeenCompleted(numberOfRows: Int): Statistics = {
    // Primero incrementamos el contador de filas
    val withIncrementedRows = copy(rowsCompleted = rowsCompleted + numberOfRows)
    
    // Después agregamos puntos por las líneas completadas si hay alguna
    if (numberOfRows > 0) 
      withIncrementedRows.addLinePoints(numberOfRows)
    else 
      withIncrementedRows
  }

  def addTimePoints(): Statistics =
    copy(score = score + 3)

  private def addLinePoints(numberOfRows: Int): Statistics =
    copy(pendingScore = pendingScore + (500 * numberOfRows * (numberOfRows + 1) / 2))

  def applyPendingPoints(): Statistics = {
    val increment = math.max(100, pendingScore / 4)
    if (pendingScore > 0) copy(score = score + increment, pendingScore = pendingScore - increment) else this
  }

  // Variable para almacenar el tiempo en pausa para esta instancia
  private var pausedTime: Long = 0
  
  def withPausedTime(newPausedTime: Long): Statistics = {
    // Actualizar el tiempo en pausa para esta instancia
    pausedTime = newPausedTime
    this
  }

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
