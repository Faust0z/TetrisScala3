package scalatetris

import javax.sound.sampled.{AudioInputStream, AudioSystem, Clip, FloatControl}

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
  
  // Volumen inicial al 70%
  private var volumeLevel: Float = 0.2f
  
  // Método para obtener el volumen actual (0.0 a 1.0)
  def getVolume: Float = volumeLevel
  
  // Método para ajustar el volumen de todos los clips (0.0 a 1.0)
  def setVolume(volume: Float): Unit = {
    // Asegurar que el volumen esté entre 0.0 y 1.0
    volumeLevel = math.min(1.0f, math.max(0.0f, volume))
    
    // Aplicar el volumen a todos los clips
    applyVolumeToClip(musicClip)
    applyVolumeToClip(gameOverClip)
    applyVolumeToClip(collisionClip)
    applyVolumeToClip(spinClip)
    applyVolumeToClip(speedClip)
    applyVolumeToClip(sideClip)
    applyVolumeToClip(completeClip)
    applyVolumeToClip(fourLineClip)
    applyVolumeToClip(pauseClip)
    applyVolumeToClip(resumeClip)
  }
  
  // Método para aplicar el volumen a un clip individual
  private def applyVolumeToClip(clip: Option[Clip]): Unit = {
    clip.foreach { c =>
      if (c.isControlSupported(FloatControl.Type.MASTER_GAIN)) {
        val gainControl = c.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
        // Convertir de escala lineal (0.0 a 1.0) a escala logarítmica de decibelios
        val gainValue = if (volumeLevel > 0) 20.0f * math.log10(volumeLevel).toFloat else gainControl.getMinimum
        // Asegurar que el valor esté dentro del rango admitido
        val clampedGainValue = math.min(gainControl.getMaximum, math.max(gainControl.getMinimum, gainValue))
        gainControl.setValue(clampedGainValue)
      }
    }
  }

  private def loadSound(filePath: String): Option[Clip] = {
    try {
      val audioInputStream: AudioInputStream = AudioSystem.getAudioInputStream(getClass.getResource(filePath))
      val clip = AudioSystem.getClip()
      clip.open(audioInputStream)
      // Aplicar volumen al cargar
      if (clip.isControlSupported(FloatControl.Type.MASTER_GAIN)) {
        val gainControl = clip.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
        val gainValue = if (volumeLevel > 0) 20.0f * math.log10(volumeLevel).toFloat else gainControl.getMinimum
        val clampedGainValue = math.min(gainControl.getMaximum, math.max(gainControl.getMinimum, gainValue))
        gainControl.setValue(clampedGainValue)
      }
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