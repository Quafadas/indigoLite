package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*

object SceneGame extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  type SceneModel = FlicFlacGameModel
  type SceneViewModel = GameSceneViewModel

  val name: SceneName = SceneName("Game")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.gameScene,
      (m, vm) => m.copy(gameScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  def subSystems = Set(SSGame("Game"))

// --- Here we have the intrusion of the hex game objects ...
// format: off
  val boardCfg = BoardConfig(
    91,                             // GWIDTH pixel width of graphic
    81,                             // GHEIGHT pixel height of graphic
    Point(260,30),                  // where the (inisible) top left hand corner of the hex grid board is positioned
    3,                              // game size
    70,                             // amount to add to a hex centre x coord to reach the vertical line of the next column
    40,                             // half the amount to add to a hex centre y coord to reach the next hexagon below
    10                              // xcoord of halfway along the top left diagonal line of first hex
  )
// format: on

  // FIXME, eventually we will calculate / fix scaleFactor and boardCfg BasePoint ...
  // ... from window dimensions supplied in main
  var scaleFactor = 1.0

  val hexBoard = HexBoard(boardCfg, scaleFactor)
  
  val pieces = Pieces(
    boardCfg,
    hexBoard
  )

  val highLighter = HighLighter(boardCfg, hexBoard, scaleFactor)

// --- End of hex game intrusion


  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] = {
    case e: MouseEvent.MouseDown =>
      e._8 match
        case MouseButton.LeftMouseButton =>
          println("MouseEventLeftButtonDown @ " + e.position)
          val clickPoint = e.position
          val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
          hexPosn match
            // Mouse Down ... position on the grid
            case Some(pos) =>
              highLighter.setPos(pos)
              highLighter.shine(true)
              pieces.deselectAllPieces()
              pieces.findPieceByPos(pos) match
                // Mouse Down ... piece found and on the grid
                case Some(piece) => piece.setSelected(true)
                case None        => ;
              end match
            // Mouse Down ... position off the grid
            case None => ;
          end match

          Outcome(model)

        case _ =>
          Outcome(model)

      end match
    // Outcome(model.update(context.delta))

    case e: MouseEvent.MouseUp =>
      e._8 match
        case MouseButton.RightMouseButton =>
          println("@@@ MouseEventRightButtonUp @ " + e.position)
          scaleFactor = scaleFactor + 0.2
          if scaleFactor >= 2.1 then scaleFactor = 0.2
          end if
          hexBoard.changeScale(scaleFactor)
          Outcome(model)

        case MouseButton.LeftMouseButton =>
          println("@@@ MouseEventLeftButtonUp @ " + e.position)
          val clickPoint = e.position
          val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
          hexPosn match
            // Mouse Up ... The position is on the hex grid
            case Some(pos) =>
              pieces.findPieceSelected() match
                // Mouse Up ... piece selected and valid position
                case Some(piece) =>
                  piece.setPosition(pos)
                  if hexBoard.isThisHexBlack(pos) == true then piece.toggleFlip()
                  end if
                // Mouse Up ... no piece selected but on the grid
                case None => ;

            // Mouse Up ... the position is off the hex grid
            case None =>
              pieces.findPieceSelected() match
                // Mouse Up ... we have selected a piece but moved it off the grid
                case Some(piece) =>
                  piece.moveToHome()

                // Mouse Up ... no piece selected and also off the grid
                case None => ;
          end match
          // Mouse Up so turn highlighter off
          highLighter.shine(false)
          Outcome(model)

        case _ =>
          Outcome(model)
      end match
    // Outcome(model.update(context.delta))

    case _ => 
      Outcome(model)
  }
  end updateModel

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] = 
    case FrameTick =>
      viewModel.update(context.mouse, context.frameContext.inputState.pointers)

    case _ => 
      Outcome(viewModel)

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.    val x = context.

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

    val textGame = TextBox("Game Scene", 400, 40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .moveTo(20, 0)

    val bootData = context.frameContext.startUpData.flicFlacBootData

    val width = bootData.pixelWidth
    val height = bootData.pixelHeight

    Outcome {
      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.White)))
        |+| SceneUpdateFragment(textGame)
        |+| SceneUpdateFragment(viewModel.splashButton.draw)
        |+| SceneUpdateFragment(viewModel.paramsButton.draw)
//        |+| SceneUpdateFragment(viewModel.gameButton.draw)
        |+| SceneUpdateFragment(viewModel.resultsButton.draw)
        |+| hexBoard.paint(scaleFactor)
        |+| highLighter.paint(scaleFactor)
        |+| pieces.paint(scaleFactor)
    }

final case class GameSceneViewModel(
  splashButton: Button,
  paramsButton: Button,
//  gameButton: Button,
  resultsButton: Button 
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[GameSceneViewModel] =
    for {
      bn1 <- splashButton.updateFromPointers(pointers)
      bn2 <- paramsButton.updateFromPointers(pointers)
//      bn3 <- gameButton.updateFromPointers(pointers)
      bn4 <- resultsButton.updateFromPointers(pointers)
    } yield this.copy( splashButton = bn1, paramsButton = bn2, /*gameButton = bn3,*/ resultsButton = bn4)

object GameSceneViewModel:

  val initial: GameSceneViewModel = 
    GameSceneViewModel(
      Button (
        buttonAssets = GameAssets.buttonSplashAssets,
        bounds = Rectangle(20, 120, 240, 80),
        depth = Depth(6)
        ).withUpActions(ButtonSplashEvent),

      Button (
        buttonAssets = GameAssets.buttonParamsAssets,
        bounds = Rectangle(20, 220, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonParamsEvent),
/*-
      Button (
        buttonAssets = GameAssets.buttonGameAssets,
        bounds = Rectangle(20, 220, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonGameEvent),
*/
      Button (
        buttonAssets = GameAssets.buttonResultsAssets,
        bounds = Rectangle(20, 320, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)
    )

/* Adjusting buttons requires the following...
1) Add/Remove Button from Object GameSceneViewModel
2) Adjust positions of buttons in Object GameSceneViewModel
3) Add/Remove pointer handling in case class GameSceneViewModel / update
4) Add/Remove appropriate SceneUpdateFragment in present / outcome

 */    
    
