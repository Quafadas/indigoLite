package game

import indigo.*
import indigoextras.ui.*
import io.circe.Encoder
import io.circe.Decoder

final case class Piece(
    pieceShape: Int, // .................... 0=cylinder, 1=block
    pieceIdentity: Int, // ................. 0,1,2,3,4,5 for Blue/Green/Yellow/Orange/Red/Purple
    pCurPos: Point, // ..................... current position (in hexArrayCoords)
    pHomePos: Point, // .................... starting/home position (in hexArrayCoords)
    pTurnStartPos: Point, // ............... piece position at start of turn
    bTurnStartFlipState: Boolean, // ....... false if normal orientation at start of turn, true if flipped

    // parameters below required for model, but not for creation

    bFlipped: Boolean = false, // .......... piece normal is false, piece flipped is true
    bSelected: Boolean = false, // ......... piece is selected
    bCaptured: Boolean = false, // ......... piece is captured (or not)
    bCaptor: Boolean = false, // ........... piece has made a capture this turn
    bMoved: Boolean = false // ............. piece has moved this turn
) derives Encoder.AsObject,
      Decoder
object Piece:

  // --------------------------------------------------
  // Inspecting this piece ...

  def flipped(p: Piece): Boolean =
    p.bFlipped
  end flipped

  def selected(p: Piece): Boolean =
    p.bSelected
  end selected

  def captured(p: Piece): Boolean =
    p.bCaptured
  end captured

  def captor(p: Piece): Boolean =
    p.bCaptor
  end captor

  def moved(p: Piece): Boolean =
    p.bMoved
  end moved

  def position(p: Piece): Point =
    p.pCurPos
  end position

  // --------------------------------------------------
  // Manipulating this piece ...

  def setFlip(p: Piece, b: Boolean): Piece =
    p.copy(bFlipped = b)
  end setFlip

  def setSelected(p: Piece, b: Boolean): Piece =
    p.copy(bSelected = b)
  end setSelected

  def setToggleFlip(p: Piece): Piece =
    p.copy(bFlipped = if p.bFlipped then false else true)
  end setToggleFlip

  def setCaptured(p: Piece, b: Boolean): Piece =
    p.copy(bCaptured = b)
  end setCaptured

  def setCaptor(p: Piece, b: Boolean): Piece =
    p.copy(bCaptor = b)
  end setCaptor

  def setMoved(p: Piece, b: Boolean): Piece =
    p.copy(bMoved = b)
  end setMoved

  def setTurnStartPos(p: Piece, pPos: Point): Piece =
    p.copy(pTurnStartPos = pPos, bTurnStartFlipState = p.bFlipped)
  end setTurnStartPos

  def setPosition(p: Piece, pPos: Point): Piece =
    if p.pCurPos == pPos then
      // no change
      p
    else if pPos == p.pTurnStartPos then
      // piece taking back move
      p.copy(pCurPos = pPos, bMoved = false, bCaptor = false, bFlipped = p.bTurnStartFlipState)
    else
      // normal move
      p.copy(pCurPos = pPos, bMoved = true)
    end if
  end setPosition

  def moveToHome(p: Piece): Piece =
    p.copy(pCurPos = p.pHomePos)
  end moveToHome

  def setPosDeselect(p: Piece, pPos: Point): Piece =
    val p1 = setPosition(p, pPos)
    val p2 = setSelected(p1, false)
    p2
  end setPosDeselect

  def setPosFlipDeselect(p: Piece, pPos: Point): Piece =
    val p1 = setPosition(p, pPos)
    val p2 = setToggleFlip(p1)
    val p3 = setSelected(p2, false)
    p3
  end setPosFlipDeselect

  // --------------------------------------------------
  // Identifying this piece ...

  def pieceShape(p: Piece): Int =
    p.pieceShape
  end pieceShape

  def pieceId(p: Piece): Int =
    p.pieceIdentity
  end pieceId

  def pieceName(p: Piece): String =
    PieceAssets.pieceNames(p.pieceIdentity)
  end pieceName

end Piece

// Common Assets needed to build each piece
object PieceAssets:
  val gWidth = 90 // .......... GWIDTH pixel height of graphic (also known as iHexPixelWidth)
  val gHeight = 80 // ......... GHEIGHT pixel height of graphic (also known as iHexPixelHeight)
  val blocksAssetName = AssetName(GameAssets.blAssetName)
  val cylindersAssetName = AssetName(GameAssets.cyAssetName)

  val pieceTypes: Vector[String] =
    Vector("Cyldr", "Block")

  val pieceNames: Vector[String] = // Piece Names %6
    Vector("Blue  ", "Green ", "Yellow", "Orange", "Red   ", "Purple", "Grey  ")

  def getGraphic(shape: Int, id: Int, flipped: Boolean): Graphic[Material.ImageEffects] =
    val safeId = id % (6 + 1) // there are six main colours + 1 more, which is for grey (the captured color)
    val pieceAssetName = if shape == CYLINDER then cylindersAssetName else blocksAssetName
    val verticalOffset = if flipped then gHeight else 0
    val pieceRect = Rectangle(gWidth * safeId, 0 + verticalOffset, gWidth + 1, gHeight + 1)
    val pieceGraphic = Graphic(pieceRect, 4, Material.ImageEffects(pieceAssetName)) // Pieces on Layer 4
    pieceGraphic
  end getGraphic

end PieceAssets
