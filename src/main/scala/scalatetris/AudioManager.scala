package scalatetris

import javax.sound.sampled.{AudioInputStream, AudioSystem, Clip}

object AudioManager {
  private var musicClip: Option[Clip] = None
  private var gameOverClip: Option[Clip] = None
  private var collisionClip: Option[Clip] = None
  private var spinClip: Option[Clip] = None
  private var speedClip: Option[Clip] = None
  private var sideClip: Option[Clip] = None
  private var completeClip: Option[Clip] = None
  private var fourLineClip: Option[Clip] = None
  private var pauseClip: Option[Clip] = None
  private var resumeClip: Option[Clip] = None

  private def loadSound(filePath: String): Option[Clip] = {
    try {
      val audioInputStream: AudioInputStream = AudioSystem.getAudioInputStream(getClass.getResource(filePath))
      val clip = AudioSystem.getClip()
      clip.open(audioInputStream)
      Some(clip)
    } catch {
      case _: Exception => None
    }
  }

  def loadAllSounds(): Unit = {
    musicClip = loadSound("/tetris_music.wav")
    gameOverClip = loadSound("/game_over_sound.wav")
    collisionClip = loadSound("/collision.wav")
    spinClip = loadSound("/spin.wav")
    speedClip = loadSound("/sped.wav")
    sideClip = loadSound("/side.wav")
    completeClip = loadSound("/complete.wav")
    fourLineClip = loadSound("/4line.wav")
    pauseClip = loadSound("/pause.wav")
    resumeClip = loadSound("/resume.wav")
  }

  private def playEffect(clip: Option[Clip]): Unit = {
    clip.foreach { c =>
      c.setFramePosition(0)
      c.start()
    }
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
      clip.loop(Clip.LOOP_CONTINUOUSLY)
      clip.start()
    }
  }

  def playGameOverSound(): Unit = {
    playEffect(gameOverClip)
  }

  def playCollisionSound(): Unit = {
    playEffect(collisionClip)
  }

  def playSpinSound(): Unit = {
    playEffect(spinClip)
  }

  def playSpeedSound(): Unit = {
    playEffect(speedClip)
  }

  def playSideSound(): Unit = {
    playEffect(sideClip)
  }

  def playCompleteSound(): Unit = {
    playEffect(completeClip)
  }

  def playFourLineSound(): Unit = {
    playEffect(fourLineClip)
  }

  def playPauseSound(): Unit = {
    playEffect(pauseClip)
  }

  def playResumeSound(): Unit = {
    playEffect(resumeClip)
  }
}