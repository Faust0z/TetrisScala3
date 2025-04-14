package scalatetris.engine

import scalatetris.environment._
import scalatetris.AudioManager

//le paso por parametro el tamaño del tablero y la fabrica de piezas de tetris
sealed class GameEngine(val boardSize: Size, val stoneFactory: StoneFactory) {
  private var board: Board = new Board(
    boardSize,
    stoneFactory.createRandomStone(),
    stoneFactory.createRandomStone()
  )

  //isRunning: si el juego está activo o en pausa
  private var isRunning: Boolean = true
  //history: lista de tableros anteriores (para "deshacer").
  //el board es el tablero actual
  private var history: List[Board] = board :: Nil
  //future: lista de tableros a los que se puede volver (para "rehacer").
  private var future: List[Board] = Nil


  /*
  Intenta mover la pieza activa hacia abajo, sino puede moverse (colisión), fija la pieza y genera una nueva.
  si hay una fila completa la elimina con removeFullRows.
   */
  def moveDown(): Unit = {
    if (!move(_.moveDown())) {
      val (points, numberOfRemovedRows) = removeFullRows(board.points)
      board = board.update(List(Stone(points)), numberOfRemovedRows, stoneFactory.createRandomStone())
      history = board :: history
      if (!board.isGameRunning) {
        AudioManager.stopMusic()
        AudioManager.playGameOverSound()
      }
    }
  }

  //move() va a chequear si la nueva posición esta dentro del tablero y sin colisiones.
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

  //Llaman a move() con los diferentes movimientos a la pieza actual.
  //estos movimientos estan dentro de Stone donde se especifica como se mueven la piezas
  def moveLeft(): Unit = move(_.moveLeft())

  def moveRight(): Unit = move(_.moveRight())

  def rotateLeft(): Unit = move(_.rotateLeft())

  def rotateRight(): Unit = move(_.rotateRight())

  //Reinicia el tablero y limpia historial.
  def restart(): Unit = {
    board = new Board(
      boardSize,
      stoneFactory.createRandomStone(),
      stoneFactory.createRandomStone()
    )
    history = board :: Nil
    future = Nil
    isRunning = true
  }

  //fuerza la aparición de una nueva pieza.
  def forceNewStone(): Unit = {
    board = board.forceNewStone(stoneFactory.createRandomStone())
    history = board :: history
  }

  def boardIsRunning: Boolean = board.isGameRunning

  def IsRunning: Boolean = isRunning
  
  //si esta o no corriendo
  def isGameRunning: Boolean = board.isGameRunning && isRunning

  def stones: List[Stone] = board.stones

  def points: List[Point] = board.points

  def statistics: Statistics = board.statistics

  def pause(): Unit = {
    isRunning = false
    AudioManager.pauseMusic() // Pausar la música
  }

  def continue(): Unit = {
    isRunning = true
    AudioManager.resumeMusic() // Reanudar la música
    future = Nil
  }

  //esta es la proxima pieza para aparecer
  def nextStone: Stone = board.preview

  //Permite al jugador retroceder y avanzar entre estados anteriores del tablero.
  //Utiliza las listas history y future.

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

  //primero recorre de abajo hacia arriba.
  //despues chequea si una fila está llena la elimina.
  //por ultimo las filas superiores bajan una posición.
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
