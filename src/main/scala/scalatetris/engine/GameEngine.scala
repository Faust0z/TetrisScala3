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
    // No permitas movimientos si el juego ya terminó
    if (!board.isGameRunning) return

    if (!move(_.moveDown())) {
      AudioManager.playCollisionSound()
      val (points, numberOfRemovedRows) = removeFullRows(board.points)
      if (numberOfRemovedRows > 0) {
        if (numberOfRemovedRows == 4) {
          AudioManager.playFourLineSound()
        } else {
          AudioManager.playCompleteSound()
        }
      }
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
    // No permitas movimientos si el juego ya terminó
    if (!board.isGameRunning) return false

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
  def moveLeft(): Unit = {
    if (move(_.moveLeft())) {
      AudioManager.playSideSound()
    }
  }

  def moveRight(): Unit = {
    if (move(_.moveRight())) {
      AudioManager.playSideSound()
    }
  }

  /*
  Se define la rotación hacia la izquierda o la derecha según el atributo. Si es un cuadrado no se rota
   */
  private def rotate(clockwise: Boolean): Boolean = {
    val currentStone = board.stones.head
    if (currentStone.stoneType == "Square") return false

    // Función auxiliar que rota según clockwise y chequea si la piedra rotada esté en el tablero y no choque con nada
    val attemptRotation = (stone: Stone) => {
      val rotated = if (clockwise) stone.rotateRight() else stone.rotateLeft()
      if (rotated.isInFrame(board.size) && !board.stones.tail.exists(_.doesCollide(rotated)))
        Some(rotated)
      else None
    }

    // Intenta rotar la piedra hacia un lado. Si no puede (ya que el métdo devuelve None), intenta desplazarla
    // hacia el centro e intenta de nuevo hasta que consigue rotarla de alguna manera
    val rotatedStoneOpt = attemptRotation(currentStone)
      .orElse(attemptRotation(currentStone.moveRight()))
      .orElse(attemptRotation(currentStone.moveLeft()))

    rotatedStoneOpt match {
      case Some(rotatedStone) =>
        board = board.update(rotatedStone :: board.stones.tail)
        history = board :: history
        true
      case None => false
    }
  }

  def rotateLeft(): Unit = {
    if (rotate(false)) {
      AudioManager.playSpinSound()
    }
  }

  def rotateRight(): Unit = {
    if (rotate(true)) {
      AudioManager.playSpinSound()
    }
  }

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

  def boardIsRunning: Boolean = board.isGameRunning

  def IsRunning: Boolean = isRunning

  //si esta o no corriendo
  def isGameRunning: Boolean = board.isGameRunning && isRunning

  def stones: List[Stone] = board.stones

  def points: List[Point] = board.points

  def statistics: Statistics = board.statistics

  def pause(): Unit = {
    isRunning = false
    AudioManager.pauseMusic()
    AudioManager.playPauseSound()
  }

  def continue(): Unit = {
    // Solo permite continuar si el tablero todavía está en juego
    if (!board.isGameRunning) return

    isRunning = true
    AudioManager.resumeMusic()
    AudioManager.playResumeSound()
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
