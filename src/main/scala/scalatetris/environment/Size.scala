package scalatetris.environment

/** 
 * Clase que representa las dimensiones de un área rectangular.
 * 
 * Esta clase se utiliza principalmente para definir el tamaño del tablero de juego
 * y verificar los límites para el movimiento de las piezas.
 * 
 * @param width Ancho del área en unidades
 * @param height Alto del área en unidades
 */
case class Size(width: Int, height: Int)