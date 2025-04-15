package scalatetris.environment

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

case class Statistics(startTime: Date, rowsCompleted: Int, score: Int, pendingScore: Int) {

  def anotherRowHasBeenCompleted(numberOfRows: Int): Statistics =
    if (numberOfRows > 0)
      addLinePoints(numberOfRows).copy(rowsCompleted = rowsCompleted + numberOfRows)
    else
      copy(rowsCompleted = rowsCompleted + numberOfRows)

  def addTimePoints(): Statistics =
    copy(score = score + 3)

  private def addLinePoints(numberOfRows: Int): Statistics =
    copy(pendingScore = pendingScore + (500 * numberOfRows * (numberOfRows + 1) / 2))

  def applyPendingPoints(): Statistics =
    val increment = math.max(100, pendingScore / 4)
    if (pendingScore > 0) copy(score = score + increment, pendingScore = pendingScore - increment) else this

}
