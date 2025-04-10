package scalatetris.environment

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

case class Statistics(startTime: Date, rowsCompleted: Long, score: Int) {

  def anotherRowHasBeenCompleted(numberOfRows: Int): Statistics =
    val updatedStats = copy(rowsCompleted = rowsCompleted + numberOfRows)
    if (numberOfRows > 0) addLinePoints(numberOfRows) else updatedStats

  def addTimePoints(): Statistics =
    copy(score = score + 3)

  private def addLinePoints(numberOfRows: Int): Statistics =
    copy(score = score + (500 * numberOfRows * (numberOfRows + 1) / 2))

  def draw(): String = {
    val now: Date = Calendar.getInstance().getTime
    val duration: Long = now.getTime - startTime.getTime
    val format = new SimpleDateFormat("mm:ss")

    s"Rows completed: $rowsCompleted\nTime spent    : ${format.format(duration)}\nScore: $score"
  }
}
