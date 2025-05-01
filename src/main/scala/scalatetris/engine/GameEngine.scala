package scalatetris.engine

import scalatetris.components.*
import scalatetris.ui.Statistics

/**
 * Motor principal del juego Tetris que maneja toda la lógica y el estado del juego.
 *
 * Esta clase gestiona las interacciones del usuario, el movimiento de piezas, detección
 * de colisiones, aumento de nivel y dificultad, mantenimiento del historial de movimientos,
 * y todas las reglas del juego.
 *
 * @param boardSize Tamaño del tablero de juego (ancho y alto)
 */
sealed class GameEngine(val boardSize: Size) {
  /** Estado actual del tablero con piezas, puntuación y estadísticas */
  private var board: Board = new Board(
    boardSize,
    StoneFactory.createRandomStone(),
    StoneFactory.createRandomStone()
  )

  /** Nivel actual del juego (0-29), determina la velocidad de caída de piezas */
  private var currentLevel: Int = 0

  
  /** Pieza guardada/reservada para uso posterior (función "hold") */
  private var holdStone: Option[Stone] = None

  /** Indica si la función "hold" ya se usó en este turno (se resetea al fijar una pieza) */
  private var holdUsedThisTurn: Boolean = false

  /** Indica si el juego está activo (true) o en pausa (false) */
  private var isRunning: Boolean = true
  
  /** Timestamp de la última pausa para calcular el tiempo pausado */
  private var pauseStartTime: Option[Long] = None

  /** Tiempo total que el juego ha estado en pausa (en milisegundos) */
  private var totalPausedTime: Long = 0
  //Obtiene el tiempo de juego
  private val gameStartTime: Long = System.currentTimeMillis()

  /**
   * Intenta mover la pieza activa hacia abajo. Si no puede moverse (colisión),
   * fija la pieza, elimina filas completas, actualiza puntuación y genera una nueva pieza.
   *
   * Este método es el núcleo del ciclo de juego, gestionando:
   * - Movimiento hacia abajo
   * - Fijación de piezas al llegar al fondo
   * - Eliminación de filas completas
   * - Actualización de nivel basado en filas eliminadas
   * - Comprobación de fin de juego
   */
  def moveDown(): Boolean = {
    if (!board.isGameRunning) return false

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

        board = board.update(List(Stone(points)), numberOfRemovedRows, StoneFactory.createRandomStone())
        holdUsedThisTurn = false // Reset del hold para la nueva pieza

        if (!board.isGameRunning) {
          AudioManager.stopMusic()
          AudioManager.playGameOverSound()
        }
        false
      } else {
        true
      }
  }

  
  /**
   * Actualiza el nivel del juego basado en el número de filas completadas acumuladas.
   *
   * El nivel aumenta cada 10 filas completadas hasta un máximo de nivel 29.
   * Cada nivel incrementa la velocidad de caída de las piezas.
   *
   * @param rowsCleared Número de filas eliminadas en esta jugada
   */
  private def updateLevel(rowsCleared: Int): Unit = {
    val totalRows = board.statistics.rowsCompleted + rowsCleared
    val newLevel = math.min((totalRows / 10), 29)
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
   * Este factor determina qué tan rápido caen las piezas, siguiendo una
   * curva de dificultad inspirada en la versión NES de Tetris pero adaptada:
   * - Niveles 0-9: La velocidad aumenta linealmente
   * - Niveles 10-19: La velocidad aumenta más lentamente
   * - Niveles 20-28: Pequeños ajustes de velocidad
   * - Nivel 29: "Kill screen" con caída instantánea
   *
   * @return Valor que representa la velocidad (valores más bajos = mayor velocidad)
   */
  def getSpeedFactor: Int = {
    // Obtiene el tiempo total jugado (en milisegundos) y lo convierte en minutos
    val elapsedMinutes = obtenerTiempoDeJuego / 60000

    val timeBasedSpeedBonus = (elapsedMinutes * 8).toInt

    val baseSpeed = if (currentLevel < 10) {
      48 - currentLevel * 5
    } else if (currentLevel < 20) {
      28 - (currentLevel - 10) * 2
    } else if (currentLevel < 29) {
      8 - (currentLevel - 20) / 3
    } else {
      1
    }

    math.max(baseSpeed - timeBasedSpeedBonus, 1)
  }

  def obtenerTiempoDeJuego: Long = {
    val now = System.currentTimeMillis()

    if (!isRunning && pauseStartTime.isDefined) {
      (pauseStartTime.get - gameStartTime) - totalPausedTime
    } else {
      (now - gameStartTime) - totalPausedTime
    }
  }

  /**
   * Intenta mover la pieza activa según la acción especificada.
   *
   * Verifica que el movimiento sea válido (dentro del tablero y sin colisiones)
   * antes de aplicarlo. Si es válido, actualiza el estado del juego y el historial.
   *
   * Este método es un ejemplo de función de orden superior, ya que recibe otra función
   * como parámetro.
   *
   * @param action Función que transforma una pieza (por ejemplo, moveLeft, moveRight, etc.)
   * @return true si el movimiento fue exitoso, false si fue bloqueado
   */
  private def move(action: Stone => Stone): Boolean = {
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

  /**
   * Mueve la pieza activa a la izquierda si es posible.
   *
   * Reproduce un efecto de sonido al mover lateralmente.
   */
  def moveLeft(): Unit = {
    if (move(_.moveLeft())) {
      AudioManager.playSideSound()
    }
  }

  /**
   * Mueve la pieza activa a la derecha si es posible.
   *
   * Reproduce un efecto de sonido al mover lateralmente.
   */
  def moveRight(): Unit = {
    if (move(_.moveRight())) {
      AudioManager.playSideSound()
    }
  }

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

  def holdCurrentStone(): Unit = {
    if (!board.isGameRunning || holdUsedThisTurn) return

    val currentStone = board.stones.head

    // Si ya hay una pieza en hold, intercambiar
    if (holdStone.isDefined) {
      val newCurrentStone = holdStone.get.toTopCenter(Point(board.size.width / 2, 0))
      holdStone = Some(currentStone.resetPosition())

      if (!board.stones.tail.exists(_.doesCollide(newCurrentStone))) {
        val remainingStones = board.stones.tail
        board = board.updateStones(newCurrentStone :: remainingStones)
        holdUsedThisTurn = true
      }
    } else {
      // Primera vez que se usa hold
      holdStone = Some(currentStone.resetPosition())
      val remainingStones = board.stones.tail
      board = board.updateStones(remainingStones)
      board = board.forceNewStone(StoneFactory.createRandomStone())
      holdUsedThisTurn = true
    }

    AudioManager.playSideSound()
  }
  
  /**
   * Obtener la pieza guardada con la función "hold".
   *
   * @return La pieza guardada, o None si no hay ninguna
   */
  def getHoldStone: Option[Stone] = holdStone

  /**
   * Reinicia el tablero y limpia el historial.
   *
   * Reinicia todos los aspectos del juego a sus valores iniciales:
   * - Nuevo tablero vacío
   * - Estadísticas a cero
   * - Nivel a cero
   * - Elimina pieza guardada
   * - Limpia historial
   */
  def restart(): Unit = {
    board = new Board(
      boardSize,
      StoneFactory.createRandomStone(),
      StoneFactory.createRandomStone()
    )
    isRunning = true
    currentLevel = 0
    holdStone = None
    holdUsedThisTurn = false
    totalPausedTime = 0
    pauseStartTime = None
  }

  /**
   * Verifica si el tablero está en condición de juego activo.
   *
   * @return true si el juego no ha terminado, false si es game over
   */
  def boardIsRunning: Boolean = board.isGameRunning

  /**
   * Verifica si el juego está en ejecución o pausado.
   *
   * @return true si el juego está pausado, false si está en ejecución
   */
  def IsRunning: Boolean = isRunning
  
  /**
   * Verifica si el juego está en ejecución activa.
   *
   * @return true si el juego está corriendo y el tablero es válido
   */
  def isGameRunning: Boolean = board.isGameRunning && isRunning

  /**
   * Obtiene la lista de todas las piezas en el tablero.
   *
   * @return Lista de piezas, donde la primera es la pieza activa
   */
  def stones: List[Stone] = board.stones

  /**
   * Obtiene la lista de todos los puntos ocupados en el tablero.
   *
   * @return Lista de puntos ocupados por todas las piezas
   */
  def points: List[Point] = board.points

  /**
   * Obtiene las estadísticas actuales del juego, ajustando el tiempo de pausa.
   *
   * @return Estadísticas actualizadas con tiempo de pausa aplicado
   */
  def statistics: Statistics = {
    val now = System.currentTimeMillis()

    if (!isRunning && pauseStartTime.isDefined) {
      val pauseDuration = now - pauseStartTime.get
      board.statistics.withPausedTime(totalPausedTime + pauseDuration)
    } else {
      board.statistics.withPausedTime(totalPausedTime)
    }
  }

  /**
   * Pausa el juego y registra el tiempo de inicio de la pausa.
   *
   * Detiene la acción del juego y la música, permitiendo acceder
   * a funciones como el historial.
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
   * Continúa el juego después de una pausa, actualizando el tiempo total pausado.
   *
   * Solo permite continuar si el tablero todavía está en juego.
   */
  def continue(): Unit = {
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

  /**
   * Devuelve la próxima pieza que aparecerá en el juego.
   *
   * @return La pieza que se mostrará en la vista previa
   */
  def nextStone: Stone = board.preview

  /**
   * Elimina filas completas y hace caer las piezas superiores.
   *
   * Recorre el tablero de abajo hacia arriba, verificando si cada fila
   * está completa. Si una fila está llena, la elimina y hace caer todas
   * las filas superiores una posición. Este método usa recursión para
   * procesar cada fila.
   *
   * @param points Lista de puntos ocupados en el tablero
   * @param height Altura actual que se está verificando
   * @return Tupla con (nueva lista de puntos, número de filas eliminadas)
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