package scalatetris

import scala.swing.TextArea

trait Display {
  def render(value: String): Unit
}

class SwingDisplay(area: TextArea) extends Display {
  def render(value: String) : Unit = {
      area.text = value
  }
}