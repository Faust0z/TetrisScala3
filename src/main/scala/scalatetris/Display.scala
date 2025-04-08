package scalatetris

import scala.swing.TextArea

trait Display {
  def render(value: String);
}

class SwingDisplay(area: TextArea) extends Display {
  def render(value: String) {
      area.text = value;
  }
}