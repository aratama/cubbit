module Game.Cubbit.Main (main) where

import Control.Alternative (when)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.HTMLElement (setTitle)
import DOM.HTML.Location (hostname)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document, location)
import DOM.Node.Element (setClassName)
import Data.Maybe (Maybe(Nothing))
import Data.Nullable (toMaybe, toNullable)
import Data.Set (empty)
import Data.Traversable (for_)
import Data.Unit (Unit)
import Game.Cubbit.Config (Config(Config), readConfig)
import Game.Cubbit.Hud.Driver (initializeHud)
import Game.Cubbit.Hud.Type (PlayingSceneQuery(..), Query(..), QueryA(..))
import Game.Cubbit.Terrain (createTerrain)
import Game.Cubbit.Types (Effects, SceneState(LoadingSceneState), State(State))
import Graphics.Cannon (createWorld)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Util (runHalogenAff)
import Halogen.Query (action)
import PointerLock (addPointerlockchangeListener, addPointerMoveListener)
import Prelude (negate, ($), (<<<), (==), (>>=), (>>>))
import Raven (installRaven)
import Unsafe.Coerce (unsafeCoerce)
import Graphics.Babylon.Engine (getDeltaTime, resize, runRenderLoop)

main :: forall eff. Eff (Effects eff) Unit
main = do
    installRaven "https://da2118a331e045bf9c23882ddea0a172@sentry.io/125164"

    window >>= document >>= (unsafeCoerce >>> setTitle "Cubbit×Cubbit Playable Demo")

    runHalogenAff do

        Config config <- liftEff $ readConfig

        world <- liftEff $ createWorld    -- cannon

        -- tweak for nicovideo
        host <- liftEff $ window >>= location >>= hostname
        let niconico = host == "html5.nicogame.jp"
        when niconico $ liftEff do
            b <- window >>= document >>= body
            for_ (toMaybe b) $ setClassName "niconico" <<< htmlElementToElement

        -- initialize game state
        let terrainSeed = 0
        initialTerrain <- liftEff $ createTerrain terrainSeed
        let initialState =  {
                config: Config config,
                configVisible: false,
                sceneState: LoadingSceneState 0,
                nextScene: false,
                skyboxRotation: 0.0,
                terrain: initialTerrain,
                updateIndex: toNullable Nothing,
                world,
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

        -- initialize ui
        bodyElement <- awaitBody
        driver <- initializeHud (State initialState) bodyElement

        liftEff do
            addPointerlockchangeListener \element -> runHalogenAff do
                driver.query $ action $ Query $ PlayingSceneQuery $ OnChangePointerlock $ toMaybe element
            addPointerMoveListener \e -> runHalogenAff do
                driver.query $ action $ Query $ PlayingSceneQuery $ OnMovePointer e

        -- **HACK** put it the last of the sequence
        driver.query $ action $ Query Gameloop



