package scalatetris.engine

import scalatetris.environment._
import scalatetris.AudioManager
import scala.util.Try

//le paso por parametro el tamaño del tablero y la fabrica de piezas de tetris
sealed class GameEngine(val boardSize: Size, val stoneFactory: StoneFactory) {
  private var board: Board = new Board(
    boardSize,
    stoneFactory.createRandomStone(),
    stoneFactory.createRandomStone()
  )

  // Nivel actual del juego (0-29)
  private var currentLevel: Int = 0
  
  // Pieza guardada/reservada (hold)
  private var holdStone: Option[Stone] = None
  private var holdUsedThisTurn: Boolean = false
  
  // Offset para intentar rotar piezas cerca de los bordes (wall-kicks)
  private val kickOffsets = List(
    (0, 0),   // Sin desplazamiento
    (1, 0),   // Mover a la derecha
    (-1, 0),  // Mover a la izquierda
    (0, -1),  // Mover hacia arriba
    (2, 0),   // Mover dos a la derecha
    (-2, 0)   // Mover dos a la izquierda
  )

  //isRunning: si el juego está activo o en pausa
  private var isRunning: Boolean = true
  //history: lista de tableros anteriores (para "deshacer").
  //el board es el tablero actual
  private var history: List[Board] = board :: Nil
  //future: lista de tableros a los que se puede volver (para "rehacer").
  private var future: List[Board] = Nil
  
  // Timestamp de la última pausa para pausar el timer
  private var pauseStartTime: Option[Long] = None
  private var totalPausedTime: Long = 0


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
      
      // Actualizar nivel según filas completadas
      if (numberOfRemovedRows > 0) {
        updateLevel(numberOfRemovedRows)
        if (numberOfRemovedRows == 4) {
          AudioManager.playFourLineSound()
        } else {
          AudioManager.playCompleteSound()
        }
      }
      
      board = board.update(List(Stone(points)), numberOfRemovedRows, stoneFactory.createRandomStone())
      history = board :: history
      holdUsedThisTurn = false  // Reset del hold para la nueva pieza
      
      if (!board.isGameRunning) {
        AudioManager.stopMusic()
        AudioManager.playGameOverSound()
      }
    }
  }
  
  // Actualizar el nivel basado en las líneas completadas
  private def updateLevel(rowsCleared: Int): Unit = {
    val totalRows = board.statistics.rowsCompleted + rowsCleared
    val newLevel = math.min((totalRows / 10).toInt, 29) // Máximo nivel 29
    if (newLevel > currentLevel) {
      currentLevel = newLevel
    }
  }
  
  // Obtiene el nivel actual
  def getLevel: Int = currentLevel
  
  // Obtiene el factor de velocidad basado en el nivel actual
  def getSpeedFactor: Int = {
    // Fórmula basada en la velocidad del NES Tetris con algunas modificaciones
    if (currentLevel < 10) {
      48 - currentLevel * 5  // Disminuye de 48 a 3 frames
    } else if (currentLevel < 20) {
      28 - (currentLevel - 10) * 2  // Disminuye de 28 a 8 frames
    } else if (currentLevel < 29) {
      8 - (currentLevel - 20) / 3  // Disminuye de 8 a 5 frames
    } else {
      1  // "Kill screen" - caída inmediata en nivel 29
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

  // Implementación mejorada de rotación con wall-kicks
  def rotateLeft(): Unit = {
    val didRotate = rotateWithKicks(stone => stone.rotateLeft())
    if (didRotate) {
      AudioManager.playSpinSound()
    }
  }

  def rotateRight(): Unit = {
    val didRotate = rotateWithKicks(stone => stone.rotateRight())
    if (didRotate) {
      AudioManager.playSpinSound()
    }
  }
  
  // Implementación de wall-kicks
  private def rotateWithKicks(rotateFunc: Stone => Stone): Boolean = {
    if (!board.isGameRunning) return false
    
    val currentStone = board.stones.head
    
    // Intentar cada offset posible
    for ((xOffset, yOffset) <- kickOffsets) {
      val rotatedStone = rotateFunc(currentStone)
      val adjustedStone = Try {
        (0 until xOffset.abs).foldLeft(rotatedStone) { (stone, _) =>
          if (xOffset > 0) stone.moveRight() else if (xOffset < 0) stone.moveLeft() else stone
        }
      }.getOrElse(rotatedStone)
      
      val finalStone = Try {
        (0 until yOffset.abs).foldLeft(adjustedStone) { (stone, _) =>
          if (yOffset < 0) stone.moveUp() else if (yOffset > 0) stone.moveDown() else stone
        }
      }.getOrElse(adjustedStone)
      
      // Verificar si esta posición es válida
      if (finalStone.isInFrame(board.size) && !board.stones.tail.exists(_.doesCollide(finalStone))) {
        board = board.update(finalStone :: board.stones.tail)
        history = board :: history
        return true
      }
    }
    
    false  // No se pudo rotar con ningún offset
  }
  
  // Hold/guardar pieza actual
  def holdCurrentStone(): Unit = {
    if (!board.isGameRunning || holdUsedThisTurn) return
    
    val currentStone = board.stones.head
    
    // Si ya hay una pieza en hold, intercambiar
    if (holdStone.isDefined) {
      val newCurrentStone = holdStone.get.toTopCenter(Point(board.size.width / 2, 0))
      holdStone = Some(currentStone.resetPosition())
      
      // Actualizar el tablero con la nueva pieza actual
      if (!board.stones.tail.exists(_.doesCollide(newCurrentStone))) {
        // Crear una nueva lista de piedras sin la actual
        val remainingStones = board.stones.tail
        // Agregar la piedra del hold como la activa
        board = board.updateStones(newCurrentStone :: remainingStones)
        history = board :: history
        holdUsedThisTurn = true
      }
    } else {
      // Primera vez que se usa hold
      holdStone = Some(currentStone.resetPosition())
      // Actualizar el tablero solo con las piezas fijas
      val remainingStones = board.stones.tail
      board = board.updateStones(remainingStones)
      board = board.forceNewStone(stoneFactory.createRandomStone())
      history = board :: history
      holdUsedThisTurn = true
    }
    
    // Reproducir un sonido cuando se guarda una pieza
    AudioManager.playSideSound()
  }
  
  // Obtener la pieza guardada
  def getHoldStone: Option[Stone] = holdStone

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
    currentLevel = 0
    holdStone = None
    holdUsedThisTurn = false
    totalPausedTime = 0
    pauseStartTime = None
  }

  //fuerza la aparición de una nueva pieza.
  def forceNewStone(): Unit = {
    if (!board.isGameRunning) return
    
    board = board.forceNewStone(stoneFactory.createRandomStone())
    history = board :: history
  }

  def boardIsRunning: Boolean = board.isGameRunning

  def IsRunning: Boolean = isRunning
  
  //si esta o no corriendo
  def isGameRunning: Boolean = board.isGameRunning && isRunning

  def stones: List[Stone] = board.stones

  def points: List[Point] = board.points

  def statistics: Statistics = {
    val now = System.currentTimeMillis()
    
    if (!isRunning && pauseStartTime.isDefined) {
      // Si estamos en pausa y aún no hemos actualizado totalPausedTime para esta pausa
      val pauseDuration = now - pauseStartTime.get
      board.statistics.withPausedTime(totalPausedTime + pauseDuration)
    } else {
      // Si no estamos en pausa, o si ya actualizamos totalPausedTime
      board.statistics.withPausedTime(totalPausedTime)
    }
  }

  def pause(): Unit = {
    if (isRunning) {
      isRunning = false
      pauseStartTime = Some(System.currentTimeMillis())
      AudioManager.pauseMusic()
      AudioManager.playPauseSound()
    }
  }

  def continue(): Unit = {
    // Solo permite continuar si el tablero todavía está en juego
    if (!board.isGameRunning) return
    
    if (!isRunning && pauseStartTime.isDefined) {
      val now = System.currentTimeMillis()
      totalPausedTime += now - pauseStartTime.get
      pauseStartTime = None
    }
    
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
