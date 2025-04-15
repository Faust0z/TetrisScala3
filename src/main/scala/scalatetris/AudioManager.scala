package scalatetris

import javax.sound.sampled.{AudioInputStream, AudioSystem, Clip}

object AudioManager {
  private var musicClip: Option[Clip] = None
  private var gameOverClip: Option[Clip] = None

  def loadMusic(filePath: String): Unit = {
    val audioInputStream: AudioInputStream = AudioSystem.getAudioInputStream(getClass.getResource(filePath))
    musicClip = Some(AudioSystem.getClip())
    musicClip.foreach(_.open(audioInputStream))
  }

  def loadGameOverSound(filePath: String): Unit = {
    val audioInputStream: AudioInputStream = AudioSystem.getAudioInputStream(getClass.getResource(filePath))
    gameOverClip = Some(AudioSystem.getClip())
    gameOverClip.foreach(_.open(audioInputStream))
  }

  def playMusic(): Unit = {
    musicClip.foreach { clip =>
      clip.loop(Clip.LOOP_CONTINUOUSLY)
      clip.start()
    }
  }

  def stopMusic(): Unit = {
    musicClip.foreach(_.stop())
  }

  def pauseMusic(): Unit = {
    musicClip.foreach(_.stop())
  }

  def resumeMusic(): Unit = {
    musicClip.foreach { clip =>
      clip.start() // Solo iniciar la música sin reiniciarla
    }
  }

  def playGameOverSound(): Unit = {
    gameOverClip.foreach { clip =>
      clip.setFramePosition(0) // Reiniciar el sonido
      clip.start()
    }
  }
}