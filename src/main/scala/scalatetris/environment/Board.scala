package scalatetris.environment

import java.util.Calendar

//aca se define como funciona toda la interfaz del tablero de tetris
class Board private (
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

  //Devuelve todos los puntos ocupados en el tablero por las piezas fijadas y la actual.
  def points: List[Point] = stones.flatMap(_.points)
  //aca va a actualizar el estado del tablero con la lista de piezas
  def update(stones: List[Stone]): Board =
    new Board(size, stones, preview, statistics.addTimePoints().applyPendingPoints(), isGameRunning)

  // Método para actualizar directamente la lista de piedras
  def updateStones(stones: List[Stone]): Board =
    new Board(size, stones, preview, statistics, isGameRunning)

  //Actualiza el tablero tras eliminar filas o insertar una nueva pieza
  //Coloca la pieza preview en el centro como nueva piedra activa
  //Verifica si esa pieza colisiona con otras o está demasiado arriba entonces game over.
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

}
