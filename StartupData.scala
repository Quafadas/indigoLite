package game

import indigo.*
import indigoextras.ui.*

object FlicFlacStartupData:
  def initialise(
      flicFlacBootData: FlicFlacBootData
  ): Outcome[Startup[FlicFlacStartupData]] =
    Outcome(
      Startup.Success(createStartupData(flicFlacBootData))
    )

  def createStartupData(flicFlacBootData: FlicFlacBootData): FlicFlacStartupData =
    FlicFlacStartupData(
      flicFlacBootData = flicFlacBootData,
      staticAssets = StaticAssets(
        hexGraphic = GameAssets.gHex(1.0),
        buttonSplashAsset = GameAssets.buttonSplashAssets(1.0),
        buttonRulesAsset = GameAssets.buttonRulesAssets(1.0),
        buttonPlayAsset = GameAssets.buttonPlayAssets(1.0),
        buttonResultsAsset = GameAssets.buttonResultsAssets(1.0)
      )
    )
end FlicFlacStartupData

final case class FlicFlacStartupData(flicFlacBootData: FlicFlacBootData, staticAssets: StaticAssets)

final case class StaticAssets(
    hexGraphic: Graphic[Material.ImageEffects],
    buttonSplashAsset: ButtonAssets,
    buttonRulesAsset: ButtonAssets,
    buttonPlayAsset: ButtonAssets,
    buttonResultsAsset: ButtonAssets
)

final case class FlicFlacBootData(pixelWidth: Int, pixelHeight: Int, viewport: GameViewport):
  lazy val width = pixelWidth
  lazy val height = pixelHeight
  lazy val gameViewPort = viewport
end FlicFlacBootData

object FlicFlacBootData:
  def create(w: Int, h: Int): FlicFlacBootData =
    FlicFlacBootData(w, h, GameViewport(w, h))
end FlicFlacBootData

object FlicFlacConfig:

  lazy val config: GameConfig =
    // scribe.debug("@@@ Object FlicFlacConfigscribe.debug("@@@ Object FlicFlacConfig FINISH")")
    GameConfig(
      viewport = GameViewport(GameAssets.GameSceneDimensions.width, GameAssets.GameSceneDimensions.height),
      frameRateLimit = Option(FPS.`30`), // this is the slowest FPS indigo game engine allows ... JP 27/08/24
      clearColor = RGBA.fromHexString("#000000"),
      magnification = 1,
      transparentBackground = false,
      // the default setting from indigo is "ResizePreserveAspect" but Simon found that this corrupts the
      // css height attribute on a resize, which in turn chops off the bottom of the board.
      // The setting we discovered is simply "Resize" which keeps the width and height intact ... JP 27/08/24
      resizePolicy = ResizePolicy.Resize,
      advanced = AdvancedGameConfig(
        renderingTechnology = RenderingTechnology.WebGL2WithFallback,
        antiAliasing = false,
        premultipliedAlpha = true,
        batchSize = 256,
        autoLoadStandardShaders = true,
        disableContextMenu = true
      )
    )

end FlicFlacConfig
