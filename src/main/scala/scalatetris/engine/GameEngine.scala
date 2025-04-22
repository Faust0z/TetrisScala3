package scalatetris.engine

import scalatetris.environment._
import scalatetris.AudioManager
import scala.util.Try

/**
 * Motor principal del juego Tetris que maneja toda la lógica del juego.
 * 
 * Esta clase es responsable de:
 * - Gestionar el estado del tablero
 * - Controlar el movimiento de las piezas
 * - Manejar las colisiones
 * - Gestionar el sistema de niveles y dificultad
 * - Mantener el historial de movimientos
 * 
 * @param boardSize Tamaño del tablero de juego
 * @param stoneFactory Fábrica que genera las piezas de Tetris
 */
sealed class GameEngine(val boardSize: Size, val stoneFactory: StoneFactory) {
  /** Estado actual del tablero */
  private var board: Board = new Board(
    boardSize,
    stoneFactory.createRandomStone(),
    stoneFactory.createRandomStone()
  )

  /** Nivel actual del juego (0-29) */
  private var currentLevel: Int = 0
  
  /** Pieza guardada en el hold */
  private var holdStone: Option[Stone] = None
  /** Indica si ya se usó el hold en este turno */
  private var holdUsedThisTurn: Boolean = false

  /** Indica si el juego está activo o en pausa */
  private var isRunning: Boolean = true
  /** Lista de tableros anteriores para la función de "deshacer" */
  private var history: List[Board] = board :: Nil
  /** Lista de tableros futuros para la función de "rehacer" */
  private var future: List[Board] = Nil
  
  /** Timestamp del inicio de la última pausa */
  private var pauseStartTime: Option[Long] = None
  /** Tiempo total acumulado en pausa */
  private var totalPausedTime: Long = 0

  /** 
   * Intenta mover la pieza activa hacia abajo.
   * 
   * Si la pieza no puede moverse más (colisión), la fija en su posición
   * y genera una nueva pieza. Si hay filas completas, las elimina.
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
  
  /** 
   * Actualiza el nivel del juego basado en las líneas completadas.
   * 
   * El nivel aumenta cada 10 líneas completadas, hasta un máximo de 29.
   * 
   * @param rowsCleared Número de filas completadas en la última jugada
   */
  private def updateLevel(rowsCleared: Int): Unit = {
    val totalRows = board.statistics.rowsCompleted + rowsCleared
    val newLevel = math.min((totalRows / 10).toInt, 29) // Máximo nivel 29
    if (newLevel > currentLevel) {
      currentLevel = newLevel
    }
  }
  
  /** 
   * Obtiene el nivel actual del juego.
   * 
   * @return El nivel actual (0-29)
   */
  def getLevel: Int = currentLevel
  
  /** 
   * Calcula el factor de velocidad basado en el nivel actual.
   * 
   * La velocidad aumenta con cada nivel siguiendo una fórmula basada en el NES Tetris:
   * - Niveles 0-9: Disminuye de 48 a 3 frames
   * - Niveles 10-19: Disminuye de 28 a 8 frames
   * - Niveles 20-28: Disminuye de 8 a 5 frames
   * - Nivel 29+: "Kill screen" - caída inmediata
   * 
   * @return Factor de velocidad para el nivel actual
   */
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

  /** 
   * Verifica si un movimiento es válido y lo ejecuta.
   * 
   * @param action Función que transforma la pieza actual
   * @return true si el movimiento fue exitoso, false en caso contrario
   */
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

  /** 
   * Mueve la pieza actual hacia la izquierda si es posible.
   * Reproduce un sonido si el movimiento es exitoso.
   */
  def moveLeft(): Unit = {
    if (move(_.moveLeft())) {
      AudioManager.playSideSound()
    }
  }

  /** 
   * Mueve la pieza actual hacia la derecha si es posible.
   * Reproduce un sonido si el movimiento es exitoso.
   */
  def moveRight(): Unit = {
    if (move(_.moveRight())) {
      AudioManager.playSideSound()
    }
  }

  /** 
   * Implementa la lógica de rotación de piezas.
   * 
   * @param clockwise true para rotar en sentido horario, false para antihorario
   * @return true si la rotación fue exitosa, false en caso contrario
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

  /** 
   * Rota la pieza actual hacia la izquierda si es posible.
   * Reproduce un sonido si la rotación es exitosa.
   */
  def rotateLeft(): Unit = {
    if (rotate(false)) {
      AudioManager.playSpinSound()
    }
  }

  /** 
   * Rota la pieza actual hacia la derecha si es posible.
   * Reproduce un sonido si la rotación es exitosa.
   */
  def rotateRight(): Unit = {
    if (rotate(true)) {
      AudioManager.playSpinSound()
    }
  }
  
  /** 
   * Implementa la funcionalidad de "hold" (guardar pieza).
   * 
   * Permite guardar la pieza actual para usarla después.
   * Solo se puede usar una vez por turno (hasta que la pieza se fije).
   */
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
  
  /** 
   * Obtiene la pieza actualmente guardada en hold.
   * 
   * @return Option con la pieza guardada, o None si no hay ninguna
   */
  def getHoldStone: Option[Stone] = holdStone

  /** 
   * Reinicia el juego a su estado inicial.
   * 
   * - Limpia el tablero
   * - Reinicia el nivel
   * - Limpia el historial
   * - Reinicia el hold
   * - Reinicia los contadores de tiempo
   */
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

  /** @return true si el tablero está en estado de juego activo */
  def boardIsRunning: Boolean = board.isGameRunning

  /** @return true si el juego no está en pausa */
  def IsRunning: Boolean = isRunning

  /** @return true si el juego está activo y no en pausa */
  def isGameRunning: Boolean = board.isGameRunning && isRunning

  /** @return Lista de todas las piezas en el tablero */
  def stones: List[Stone] = board.stones

  /** @return Lista de todos los puntos ocupados en el tablero */
  def points: List[Point] = board.points

  /** 
   * Obtiene las estadísticas actuales del juego.
   * 
   * Incluye el tiempo pausado en el cálculo del tiempo total.
   * 
   * @return Estadísticas actualizadas
   */
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

  /** 
   * Pausa el juego.
   * 
   * - Detiene la música
   * - Guarda el timestamp de inicio de pausa
   * - Reproduce el sonido de pausa
   */
  def pause(): Unit = {
    if (isRunning) {
      isRunning = false
      pauseStartTime = Some(System.currentTimeMillis())
      AudioManager.pauseMusic()
      AudioManager.playPauseSound()
    }
  }

  /** 
   * Continúa el juego desde la pausa.
   * 
   * - Actualiza el tiempo total en pausa
   * - Reanuda la música
   * - Reproduce el sonido de continuar
   */
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

  /** @return La siguiente pieza que aparecerá */
  def nextStone: Stone = board.preview

  /** 
   * Retrocede un estado en el historial del juego.
   * 
   * Mueve el estado actual a la lista de futuros y
   * carga el estado anterior de la historia.
   */
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

  /** 
   * Avanza un estado en el historial del juego.
   * 
   * Mueve el estado actual a la historia y
   * carga el siguiente estado de la lista de futuros.
   */
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

  /** 
   * Elimina las filas completas del tablero.
   * 
   * Recorre el tablero de abajo hacia arriba, eliminando las filas
   * que están completas y haciendo caer las superiores.
   * 
   * @param points Lista de puntos ocupados en el tablero
   * @param height Altura actual siendo procesada
   * @return Tupla con los puntos actualizados y el número de filas eliminadas
   */
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
