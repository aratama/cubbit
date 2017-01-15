module Game.Cubbit.Main (main) where

import Control.Alternative (when)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Event.EventTypes (resize) as DOM
import DOM.HTML.HTMLElement (focus, setTitle)
import DOM.HTML.Location (hostname)
import DOM.HTML.Types (htmlElementToElement, htmlDocumentToNonElementParentNode, windowToEventTarget)
import DOM.HTML.Window (document, location)
import DOM.Node.Element (setClassName)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Nothing))
import Data.Nullable (toMaybe, toNullable)
import Data.Set (empty)
import Data.Traversable (for_)
import Data.Unit (Unit)
import Game.Cubbit.Config (Config(Config), readConfig)
import Game.Cubbit.Hud.Component (runGameUI)
import Game.Cubbit.Hud.Type (PlayingSceneQuery(..), Query(..), QueryA(..))
import Game.Cubbit.Terrain (createTerrain)
import Game.Cubbit.Types (Effects, SceneState(LoadingSceneState), State(State))
import Graphics.Babylon.Engine (resize, runRenderLoop)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Util (runHalogenAff)
import Halogen.Query (action, request)
import PointerLock (addPointerlockchangeListener, addPointerMoveListener)
import Prelude (negate, ($), (<<<), (==), (>>=), (>>>), (<#>), (<$>))
import Raven (installRaven)
import Unsafe.Coerce (unsafeCoerce)

main :: forall eff. Eff (Effects eff) Unit
main = do
    installRaven "https://da2118a331e045bf9c23882ddea0a172@sentry.io/125164"

    window >>= document >>= (unsafeCoerce >>> setTitle "Cubbit×Cubbit Playable Demo")

    Config config <- readConfig

    -- tweak for nicovideo
    host <- window >>= location >>= hostname
    let niconico = host == "html5.nicogame.jp"
    when niconico do
        b <- window >>= document >>= body
        for_ (toMaybe b) $ setClassName "niconico" <<< htmlElementToElement

    -- initialize game state
    let terrainSeed = 0
    terrain <- createTerrain terrainSeed

    runHalogenAff do
        -- initialize ui
        body <- awaitBody
        driver <- runGameUI body $ State {
            config: Config config,
            configVisible: false,
            sceneState: LoadingSceneState 0,
            nextScene: false,
            skyboxRotation: 0.0,
            terrain,
            updateIndex: toNullable Nothing,
            cameraPosition: { x: 10.0, y: 20.0, z: negate 10.0 },
            cameraTarget: { x: 0.5, y: 11.0, z: 0.5 },
            mousePosition: { x: 0, y: 0 },
            debugLayer: false,
            minimap: false,
            totalFrames: 0,
            keys: empty,
            bgm: Nothing,
            nextBGM: Nothing,
            volume: 1.0,
            niconico,
            gamepads: []
        }

        -- load resources
        res <- driver.query $ request LoadResources

        liftEff do

            -- prepare pointer lock event listener
            addPointerlockchangeListener \element -> runHalogenAff do
                driver.query $ action $ Query $ PlayingSceneQuery $ OnChangePointerlock $ toMaybe element

            addPointerMoveListener \e -> runHalogenAff do
                driver.query $ action $ Query $ PlayingSceneQuery $ OnMovePointer e

            -- focus the element
            (window >>= document <#> htmlDocumentToNonElementParentNode >>= getElementById (ElementId "content") <#> toMaybe) >>= traverse_ (focus <<< unsafeCoerce)

            -- add resize event listener
            (windowToEventTarget <$> window) >>= addEventListener DOM.resize (eventListener $ \_ -> resize res.engine) false

            -- state game loop
            runRenderLoop (runHalogenAff $ driver.query $ action $ Query $ Gameloop res) res.engine

