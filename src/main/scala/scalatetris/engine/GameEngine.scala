package scalatetris.engine

import scalatetris.environment._

import java.util.Calendar

sealed class GameEngine(val boardSize: Size, val stoneFactory: StoneFactory) {
  private var board =
    new Board(
      boardSize,
      stoneFactory.createRandomStone(),
      stoneFactory.createRandomStone())

  private var isRunning = true

  private var history: List[Board] = board :: Nil

  private var future: List[Board] = Nil

  def moveDown(): Unit = {
    if (!move(s => s.moveDown())) {
      val (points, numberOfRemovedRows) = removeFullRows(board.points)
      board = board.update(List(Stone(points)), numberOfRemovedRows, stoneFactory.createRandomStone())
      history = board :: history
    }
  }

  private def move(action: (Stone) => Stone) = {
    val oldStone = board.stones.head
    val newStone = action(oldStone)
    if (newStone.isInFrame(board.size) && !board.stones.tail.exists(s => s.doesCollide(newStone))) {
      board = board.update(newStone :: board.stones.tail)
      history = board :: history
      true
    }
    else false
  }

  def moveLeft(): Unit = {
    move(s => s.moveLeft())
  }

  def moveRight(): Unit = {
    move(s => s.moveRight())
  }

  def rotateLeft(): Unit = {
    move(s => s.rotateLeft())
  }

  def rotateRight(): Unit = {
    move(s => s.rotateRight())
  }

  def restart(): Unit = {
    board =
      new Board(
        boardSize,
        stoneFactory.createRandomStone(),
        stoneFactory.createRandomStone())
    history = board :: Nil
    future = Nil
  }

  def draw() =
    if (isRunning || !board.isGameRunning)
      board.draw()
    else {
      "GAME PAUSED\n" +
        board.draw()
    }

  def forceNewStone(): Unit = {
    board = board.forceNewStone(stoneFactory.createRandomStone())
    history = board :: history
  }

  def isGameRunning = board.isGameRunning && isRunning

  def stones = board.stones

  def points = board.points

  def statistics = board.statistics

  def drawBoardOnly() = board.drawBoardOnly()

  def pause() = isRunning = false

  def continue() = {
    isRunning = true
    future = Nil
  }

  def backwardInTime(): Unit = {
    history match {
      case Nil => ()
      case head :: tail =>
        future = board :: future
        board = head
        history = tail
    }
    pause()
  }

  def backIntoTheFuture(): Unit = {
    future match {
      case Nil => ()
      case head :: tail =>
        history = board :: history
        board = head
        future = tail
    }
    pause()
  }

  private def removeFullRows(points: List[Point],
                             height: Int = board.size.height): (List[Point], Int) = points match {
    case Nil => (Nil, 0)
    case _ => val (pointsInRow, pointsNotInRow) = points.partition(_.y == height)
      val (rows, numberOfRemovedRows) = removeFullRows(pointsNotInRow, height - 1)
      if (pointsInRow.length == board.size.width) {
        (rows.map(_.moveDown()), numberOfRemovedRows + 1)
      }
      else {
        (pointsInRow ::: rows, numberOfRemovedRows)
      }
  }
}
  

