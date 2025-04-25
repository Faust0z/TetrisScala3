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

  //isRunning: si el juego está activo o en pausa
  private var isRunning: Boolean = true

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

  // Se define la rotación hacia la izquierda o la derecha según el atributo. Si es un cuadrado no se rota
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
        holdUsedThisTurn = true
      }
    } else {
      // Primera vez que se usa hold
      holdStone = Some(currentStone.resetPosition())
      // Actualizar el tablero solo con las piezas fijas
      val remainingStones = board.stones.tail
      board = board.updateStones(remainingStones)
      board = board.forceNewStone(stoneFactory.createRandomStone())
      holdUsedThisTurn = true
    }
    
    // Reproducir un sonido cuando se guarda una pieza
    AudioManager.playSideSound()
  }
  
  // Obtener la pieza guardada
  def getHoldStone: Option[Stone] = holdStone

  //Reinicia el tablero.
  def restart(): Unit = {
    board = new Board(
      boardSize,
      stoneFactory.createRandomStone(),
      stoneFactory.createRandomStone()
    )
    isRunning = true
    currentLevel = 0
    holdStone = None
    holdUsedThisTurn = false
    totalPausedTime = 0
    pauseStartTime = None
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
