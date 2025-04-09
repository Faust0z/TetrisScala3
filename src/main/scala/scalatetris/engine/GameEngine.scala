package scalatetris.engine

import scalatetris.environment._

sealed class GameEngine(val boardSize: Size, val stoneFactory: StoneFactory) {
  private var board: Board = new Board(
    boardSize,
    stoneFactory.createRandomStone(),
    stoneFactory.createRandomStone()
  )

  private var isRunning: Boolean = true
  private var history: List[Board] = board :: Nil
  private var future: List[Board] = Nil

  def moveDown(): Unit = {
    if (!move(_.moveDown())) {
      val (points, numberOfRemovedRows) = removeFullRows(board.points)
      board = board.update(List(Stone(points)), numberOfRemovedRows, stoneFactory.createRandomStone())
      history = board :: history
    }
  }

  private def move(action: Stone => Stone): Boolean = {
    val oldStone = board.stones.head
    val newStone = action(oldStone)

    if (newStone.isInFrame(board.size) && !board.stones.tail.exists(_.doesCollide(newStone))) {
      board = board.update(newStone :: board.stones.tail)
      history = board :: history
      true
    } else {
      false
    }
  }

  def moveLeft(): Unit = move(_.moveLeft())

  def moveRight(): Unit = move(_.moveRight())

  def rotateLeft(): Unit = move(_.rotateLeft())

  def rotateRight(): Unit = move(_.rotateRight())

  def restart(): Unit = {
    board = new Board(
      boardSize,
      stoneFactory.createRandomStone(),
      stoneFactory.createRandomStone()
    )
    history = board :: Nil
    future = Nil
  }

  def draw(): String =
    if (isRunning || !board.isGameRunning)
      board.draw()
    else
      "GAME PAUSED\n" + board.draw()

  def forceNewStone(): Unit = {
    board = board.forceNewStone(stoneFactory.createRandomStone())
    history = board :: history
  }

  def isGameRunning: Boolean = board.isGameRunning && isRunning

  def stones: List[Stone] = board.stones

  def points: List[Point] = board.points

  def statistics: Statistics = board.statistics

  def drawBoardOnly(): String = board.drawBoardOnly()

  def pause(): Unit = isRunning = false

  def continue(): Unit = {
    isRunning = true
    future = Nil
  }

  def backwardInTime(): Unit = {
    history match {
      case Nil =>
      case head :: tail =>
        future = board :: future
        board = head
        history = tail
    }
    pause()
  }

  def backIntoTheFuture(): Unit = {
    future match {
      case Nil =>
      case head :: tail =>
        history = board :: history
        board = head
        future = tail
    }
    pause()
  }

  private def removeFullRows(points: List[Point], height: Int = board.size.height): (List[Point], Int) =
    points match {
      case Nil => (Nil, 0)
      case _ =>
        val (pointsInRow, pointsNotInRow) = points.partition(_.y == height)
        val (rows, numberOfRemovedRows) = removeFullRows(pointsNotInRow, height - 1)
        if (pointsInRow.length == board.size.width) {
          (rows.map(_.moveDown()), numberOfRemovedRows + 1)
        } else {
          (pointsInRow ::: rows, numberOfRemovedRows)
        }
    }
}
