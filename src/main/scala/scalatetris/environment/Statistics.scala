package scalatetris.environment

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

case class Statistics(startTime: Date, rowsCompleted: Long) {

  def anotherRowHasBeenCompleted(numberOfRows: Int): Statistics =
    copy(rowsCompleted = rowsCompleted + numberOfRows)

  def draw(): String = {
    val now: Date = Calendar.getInstance().getTime
    val duration: Long = now.getTime - startTime.getTime
    val format = new SimpleDateFormat("mm:ss")

    s"Rows completed: $rowsCompleted\nTime spent    : ${format.format(duration)}"
  }
}
