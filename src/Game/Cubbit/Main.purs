module Game.Cubbit.Main (main) where

import Control.Alternative (when)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef, writeRef)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Event.EventTypes (resize) as DOM
import DOM.HTML.HTMLElement (focus)
import DOM.HTML.Location (hostname)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, htmlElementToElement, windowToEventTarget)
import DOM.HTML.Window (document, location)
import DOM.Node.Element (setClassName)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(ElementId))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, toNullable)
import Data.Set (empty)
import Data.Show (show)
import Data.Traversable (for_)
import Data.Unit (Unit)
import Game.Cubbit.Collesion (createPlayerCollesion, updatePhysics)
import Game.Cubbit.Config (Config(Config), readConfig)
import Game.Cubbit.Hud.Driver (initializeHud)
import Game.Cubbit.Hud.Eval (repaint, initializeTerrain)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (loadResources, resourceCount)
import Game.Cubbit.Terrain (createTerrain)
import Game.Cubbit.Types (Effects, SceneState(..), State(State))
import Game.Cubbit.Update (update, updateBabylon, updateSound)
import Graphics.Babylon.Engine (getDeltaTime, resize, runRenderLoop)
import Graphics.Babylon.Scene (render)
import Graphics.Babylon.Util (querySelectorCanvasAff)
import Graphics.Cannon (addBody, createVec3, createWorld, setGravity)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Util (runHalogenAff)
import Prelude (negate, (#), ($), (+), (<#>), (<$>), (<<<), (<>), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

main :: forall eff. Eff (Effects eff) Unit
main = runHalogenAff do

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
            niconico
        }

    ref <- liftEff $ newRef $ State initialState

    -- initialize ui
    bodyElement <- awaitBody
    driver <- initializeHud (State initialState) ref bodyElement
    canvasGL <- querySelectorCanvasAff "#renderCanvas"

    -- load resources
    incref <- liftEff $ newRef 0
    let inc = do
            liftEff $ modifyRef incref (_ + 1)
            count <- liftEff $ readRef incref
            liftEff $ log $ show count <> " / " <> show resourceCount
            State state <- liftEff $ readRef ref
            repaint driver $ State state {
                sceneState = LoadingSceneState count
            }
    res@{ options: Options options } <- loadResources canvasGL inc
    repaint driver $ State initialState {
        nextBGM = Just res.sounds.cleaning,
        sceneState = TitleSceneState {
            res: res,
            position: 0.0
        }
    }

    liftEff do

        -- initialize cannon --
        gravity <- createVec3 0.0 options.gravity 0.0
        setGravity gravity world
        playerBox <- createPlayerCollesion
        addBody playerBox world

        -- clear terrain mesh and terrain bodies
        initializeTerrain ref res

        -- focus the element
        (window >>= document <#> htmlDocumentToNonElementParentNode >>= getElementById (ElementId "content") <#> toMaybe) >>= traverse_ (focus <<< unsafeCoerce)

        -- add resize event listener
        (windowToEventTarget <$> window) >>= addEventListener DOM.resize (eventListener $ \_ -> resize res.engine) false

        -- start game loop
        res.engine # runRenderLoop do
            State st <- readRef ref
            case st.sceneState of
                ModeSelectionSceneState _ -> readRef ref >>= updateSound res.sounds >>= writeRef ref
                _ -> do
                    deltaTime <- getDeltaTime res.engine
                    readRef ref >>=
                        update deltaTime res driver >>=
                            updateBabylon deltaTime res >>=
                                updateSound res.sounds >>=
                                    updatePhysics deltaTime playerBox world >>=
                                        writeRef ref
                    render res.scene






