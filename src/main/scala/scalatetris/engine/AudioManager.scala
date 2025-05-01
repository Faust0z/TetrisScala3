package scalatetris.engine

import javax.sound.sampled.{AudioInputStream, AudioSystem, Clip, FloatControl}

/** 
 * Objeto que gestiona todo el sistema de audio del juego.
 * 
 * Este manager se encarga de:
 * - Cargar y reproducir efectos de sonido
 * - Controlar la música de fondo
 * - Manejar el volumen global
 * - Gestionar los recursos de audio
 */
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
  
  // Volumen inicial al 20%
  private var volumeLevel: Float = 0.2f
  
  /** 
   * Obtiene el nivel de volumen actual.
   * 
   * @return Valor entre 0.0 y 1.0 que representa el volumen
   */
  def getVolume: Float = volumeLevel
  
  /** 
   * Ajusta el volumen de todos los clips de audio.
   * 
   * @param volume Nuevo nivel de volumen (entre 0.0 y 1.0)
   */
  def setVolume(volume: Float): Unit = {
    volumeLevel = math.min(1.0f, math.max(0.0f, volume))
    
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
  
  /** 
   * Aplica el nivel de volumen actual a un clip específico.
   * 
   * @param clip Clip de audio al que aplicar el volumen
   */
  private def applyVolumeToClip(clip: Option[Clip]): Unit = {
    clip.foreach { c =>
      if (c.isControlSupported(FloatControl.Type.MASTER_GAIN)) {
        val gainControl = c.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
        val gainValue = if (volumeLevel > 0) 20.0f * math.log10(volumeLevel).toFloat else gainControl.getMinimum
        val clampedGainValue = math.min(gainControl.getMaximum, math.max(gainControl.getMinimum, gainValue))
        gainControl.setValue(clampedGainValue)
      }
    }
  }

  /** 
   * Carga un archivo de sonido desde los recursos.
   * 
   * @param filePath Ruta al archivo de sonido en los recursos
   * @return Option con el Clip cargado, o None si hubo error
   */
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

  /** 
   * Carga todos los archivos de sonido necesarios para el juego.
   */
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

  /** 
   * Reproduce un efecto de sonido una sola vez.
   * 
   * @param clip Clip de audio a reproducir
   */
  private def playEffect(clip: Option[Clip]): Unit = {
    clip.foreach { c =>
      c.setFramePosition(0)
      c.start()
    }
  }

  /** 
   * Inicia la reproducción de la música en bucle.
   */
  def playMusic(): Unit = {
    musicClip.foreach { clip =>
      clip.loop(Clip.LOOP_CONTINUOUSLY)
      clip.start()
    }
  }

  /** 
   * Detiene la reproducción de la música.
   */
  def stopMusic(): Unit = {
    musicClip.foreach(_.stop())
  }

  /** 
   * Pausa la reproducción de la música.
   */
  def pauseMusic(): Unit = {
    musicClip.foreach(_.stop())
  }

  /** 
   * Reanuda la reproducción de la música en bucle.
   */
  def resumeMusic(): Unit = {
    musicClip.foreach { clip =>
      clip.loop(Clip.LOOP_CONTINUOUSLY)
      clip.start()
    }
  }

  // Métodos para reproducir efectos de sonido específicos
  
  /** Reproduce el sonido de game over */
  def playGameOverSound(): Unit = {
    playEffect(gameOverClip)
  }

  /** Reproduce el sonido de colisión */
  def playCollisionSound(): Unit = {
    playEffect(collisionClip)
  }

  /** Reproduce el sonido de rotación */
  def playSpinSound(): Unit = {
    playEffect(spinClip)
  }

  /** Reproduce el sonido de aceleración */
  def playSpeedSound(): Unit = {
    playEffect(speedClip)
  }

  /** Reproduce el sonido de movimiento lateral */
  def playSideSound(): Unit = {
    playEffect(sideClip)
  }

  /** Reproduce el sonido de línea completada */
  def playCompleteSound(): Unit = {
    playEffect(completeClip)
  }

  /** Reproduce el sonido de Tetris (4 líneas) */
  def playFourLineSound(): Unit = {
    playEffect(fourLineClip)
  }

  /** Reproduce el sonido de pausa */
  def playPauseSound(): Unit = {
    playEffect(pauseClip)
  }

  /** Reproduce el sonido de reanudar */
  def playResumeSound(): Unit = {
    playEffect(resumeClip)
  }
}