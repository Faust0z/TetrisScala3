package scalatetris.environment

/**
 * Representa el tamaño del board. Implementado como case class para permitir la comparación entre tableros y sus tamaños.
 *
 * @param width  Ancho del board (cantidad de columnas)
 * @param height Alto del board (cantidad de filas)
 */
case class Size(width: Int, height: Int)