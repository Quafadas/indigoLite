package game

import indigo.*

final case class ScorePanel():
  val p0 = Point(0, 130) // ....... coords of score panel on the game scene when 100% zoom 
  val p1 = Point(14, 190) //....... coords of cylinder name
  val p2 = Point(14, 370) // ...... coords of block name
  val p3a = Point(150, 250) // .... coords of cylinder score if single digit
  val p3b = Point(120, 250) // .... coords of cylinder score if 10 or more
  val p4a = Point(150, 430) // .... coords of block score if single digit
  val p4b = Point(120, 430) // .... coords of block score if 10 or more

  def show(model: FlicFlacGameModel, bBlinkOn: Boolean, dSF: Double): Layer =

    val p0Scaled = p0 * dSF
    val p1Scaled = p1 * dSF
    val p2Scaled = p2 * dSF
    val p3Scaled = coordsFromScore(model.gameScore._1, p3a, p3b) * dSF
    val p4Scaled = coordsFromScore(model.gameScore._2, p4a, p4b) * dSF

    val cylinderName =
      if model.ourPieceType == CYLINDER then model.ourName
      else model.oppoName
      end if
    end cylinderName

    val blockName =
      if model.ourPieceType == CYLINDER then model.oppoName
      else model.ourName
      end if
    end blockName

    val hybridScorePanel =
      if (bBlinkOn == true) && (model.gameState == GameState.CYLINDER_TURN) then
        // select purple cylinder icon
        GameAssets.gScorePanelBlinkCylinder(dSF).moveTo(p0Scaled)
      else if (bBlinkOn == true) && (model.gameState == GameState.BLOCK_TURN) then
        // select purple block icon
        GameAssets.gScorePanelBlinkBlock(dSF).moveTo(p0Scaled)
      else
        // normal panel
        GameAssets.gScorePanelBlinkOff(dSF).moveTo(p0Scaled)
      end if
    end hybridScorePanel

    val cylinderPlayer =
      TextBox((cylinderName).toString(), 220, 50)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(40))
        .scaleBy(dSF, dSF)
        .moveTo(p1Scaled)

    val blockPlayer =
      TextBox((blockName).toString(), 220, 50)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(40))
        .scaleBy(dSF, dSF)
        .moveTo(p2Scaled)

    val cylinderScore =
      TextBox((model.gameScore._1).toString(), 150, 300)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(100))
        .scaleBy(dSF, dSF)
        .moveTo(p3Scaled)

    val blockScore =
      TextBox((model.gameScore._2).toString(), 150, 300)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(100))
        .scaleBy(dSF, dSF)
        .moveTo(p4Scaled)

    val content1 = Layer(hybridScorePanel)
    val content2 = Layer(cylinderPlayer)
    val content3 = Layer(blockPlayer)
    val content4 = Layer(cylinderScore)
    val content5 = Layer(blockScore)

    (content1 |+| content2 |+| content3 |+| content4 |+| content5)
  end show

  def coordsFromScore(score: Int, coordsForOneDigit: Point, coordsForTwoDigits: Point ): Point =
    if score < 10 then coordsForOneDigit
    else coordsForTwoDigits
    end if
  end coordsFromScore

end ScorePanel
