package scalatetris.environment

import java.util.Calendar

class Board private(
                     val size: Size,
                     val stones: List[Stone],
                     val preview: Stone,
                     val statistics: Statistics,
                     val isGameRunning: Boolean) {


  def this(size: Size, firstStone: Stone, firstPreview: Stone) =
    this(
      size,
      List(firstStone.toTopCenter(Point(size.width / 2, 0))),
      firstPreview,
      Statistics(Calendar.getInstance().getTime, 0, 0, 0),
      isGameRunning = true
    )

  private def topCenter: Point = Point(size.width / 2, 0)

  def points: List[Point] = stones.flatMap(_.points)

  def update(stones: List[Stone]): Board =
    new Board(size, stones, preview, statistics.addTimePoints().applyPendingPoints(), isGameRunning)

  def update(stones: List[Stone], numberOfRowsRemoved: Int, preview: Stone): Board = {
    val gameOver = stones.exists(_.doesCollide(this.preview)) || stones.headOption.exists(_.isOnTop)

    new Board(
      size,
      if (gameOver) stones else this.preview.toTopCenter(topCenter) :: stones,
      preview,
      if (gameOver) statistics else statistics.anotherRowHasBeenCompleted(numberOfRowsRemoved),
      isGameRunning = !gameOver
    )
  }

  def forceNewStone(preview: Stone): Board =
    new Board(size, this.preview.toTopCenter(topCenter) :: stones, preview, statistics, isGameRunning)

  def draw(): String = {
    val previewSize = Size(5, 5)
    val previewStone = preview.toTopCenter(Point(previewSize.width / 2, 0))

    val boardDrawing = drawBoardOnlyInternal(size, points)
    val previewDrawing = drawBoardOnlyInternal(previewSize, previewStone.points)
    val gameStatus = if (!isGameRunning) "GAME OVER\n" else ""

    s"$boardDrawing\n$previewDrawing\n$gameStatus${statistics.draw()}"
  }

  def drawBoardOnly(): String = drawBoardOnlyInternal(size, points)

  private def drawBoardOnlyInternal(size: Size, points: List[Point]): String = {
    val occupiedPoints = points.map(p => (p.x, p.y)).toSet
    val builder = new StringBuilder

    for (y <- 0 until size.height) {
      builder.append("|")
      for (x <- 0 until size.width) {
        builder.append(if (occupiedPoints.contains((x, y))) "x" else " ")
      }
      builder.append("|\n")
    }
    builder.append("-" * (size.width + 2) + "\n")
    builder.toString()
  }
}
