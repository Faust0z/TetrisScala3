package scalatetris.components

import scalatetris.ui.Statistics

import java.util.Calendar

/** 
 * Clase que representa el tablero de juego de Tetris.
 * 
 * Esta clase es inmutable y maneja:
 * - El estado actual del tablero
 * - Las piezas activas y fijas
 * - La siguiente pieza (preview)
 * - Las estadísticas del juego
 * - El estado de juego (activo/terminado)
 */
class Board private (
                      val size: Size,
                      val stones: List[Stone],
                      val preview: Stone,
                      val statistics: Statistics,
                      val isGameRunning: Boolean) {

  /** 
   * Constructor principal que inicializa un nuevo tablero.
   * 
   * @param size Dimensiones del tablero
   * @param firstStone Primera pieza activa
   * @param firstPreview Primera pieza en preview
   */
  def this(size: Size, firstStone: Stone, firstPreview: Stone) =
    this(
      size,
      List(firstStone.toTopCenter(Point(size.width / 2, 0))),
      firstPreview,
      Statistics(Calendar.getInstance().getTime, 0, 0, 0),
      isGameRunning = true
    )

  /** 
   * Calcula el punto central superior del tablero.
   * 
   * @return Punto en el centro de la fila superior
   */
  private def topCenter: Point = Point(size.width / 2, 0)

  /** 
   * Obtiene todos los puntos ocupados en el tablero.
   * 
   * @return Lista de puntos ocupados por todas las piezas
   */
  def points: List[Point] = stones.flatMap(_.points)

  /** 
   * Actualiza el estado del tablero con una nueva lista de piezas.
   * 
   * @param stones Nueva lista de piezas
   * @return Nuevo tablero con las piezas actualizadas y estadísticas incrementadas
   */
  def update(stones: List[Stone]): Board =
    new Board(size, stones, preview, statistics.addTimePoints().applyPendingPoints(), isGameRunning)

  /** 
   * Actualiza directamente la lista de piezas sin modificar otros estados.
   * 
   * @param stones Nueva lista de piezas
   * @return Nuevo tablero con las piezas actualizadas
   */
  def updateStones(stones: List[Stone]): Board =
    new Board(size, stones, preview, statistics, isGameRunning)

  /** 
   * Actualiza el tablero después de eliminar filas o insertar una nueva pieza.
   * 
   * Este método:
   * - Coloca la pieza preview en el centro como nueva pieza activa
   * - Verifica si hay game over (colisión o pieza en fila superior)
   * - Actualiza las estadísticas según las filas eliminadas
   * 
   * @param stones Lista de piezas fijas
   * @param numberOfRowsRemoved Número de filas que se eliminaron
   * @param preview Nueva pieza para el preview
   * @return Nuevo tablero actualizado
   */
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

  /** 
   * Fuerza la aparición de una nueva pieza en el tablero.
   * 
   * @param preview Nueva pieza para el preview
   * @return Nuevo tablero con la nueva pieza activa
   */
  def forceNewStone(preview: Stone): Board =
    new Board(size, this.preview.toTopCenter(topCenter) :: stones, preview, statistics, isGameRunning)

}
