package scalatetris.environment

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

case class Statistics(startTime: Date, rowsCompleted: Long, score: Int, pendingScore: Int) {

  def anotherRowHasBeenCompleted(numberOfRows: Int): Statistics =
    val updatedStats = copy(rowsCompleted = rowsCompleted + numberOfRows)
    if (numberOfRows > 0) addLinePoints(numberOfRows) else updatedStats

  def addTimePoints(): Statistics =
    copy(score = score + 3)

  private def addLinePoints(numberOfRows: Int): Statistics =
    copy(pendingScore = pendingScore + (500 * numberOfRows * (numberOfRows + 1) / 2))

  def applyPendingPoints(): Statistics =
    val increment = math.max(100, pendingScore / 4)
    if (pendingScore > 0) copy(score = score + increment, pendingScore = pendingScore - increment) else this

  def draw(): String = {
    val now: Date = Calendar.getInstance().getTime
    val duration: Long = now.getTime - startTime.getTime
    val format = new SimpleDateFormat("mm:ss")
    var displayPendingScore = ""
    if (pendingScore > 0) displayPendingScore = f" +$pendingScore" else ""

    s"Rows completed: $rowsCompleted\nTime spent: ${format.format(duration)}\nScore: $score $displayPendingScore"
  }
}
