module Game.Cubbit.Main (main) where

import Control.Alternative (when)
import Control.Bind (bind, join)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef, writeRef)
import Control.MonadPlus (guard)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Event.EventTypes (resize) as DOM
import DOM.HTML.HTMLElement (focus, setTitle)
import DOM.HTML.Location (hostname)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, htmlElementToElement, windowToEventTarget)
import DOM.HTML.Window (document, location)
import DOM.Node.Element (setClassName)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(ElementId))
import Data.Array (findIndex, length, (!!))
import Data.BooleanAlgebra (not)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Just, Nothing))
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
import Game.Cubbit.Types (Effects, GameMode(..), SceneState(..), State(State))
import Game.Cubbit.Update (update, updateBabylon, updateSound)
import Gamepad (Gamepad(..), GamepadButton(..), getGamepads)
import Graphics.Babylon.Engine (getDeltaTime, resize, runRenderLoop)
import Graphics.Babylon.Scene (render)
import Graphics.Babylon.Util (querySelectorCanvasAff)
import Graphics.Cannon (addBody, createVec3, createWorld, setGravity)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Util (runHalogenAff)
import Prelude (mod, negate, (#), ($), (+), (<#>), (<$>), (<<<), (>>>), (<>), (==), (>>=), (&&))
import Raven (install)
import Unsafe.Coerce (unsafeCoerce)

main :: forall eff. Eff (Effects eff) Unit
main = do
    install "https://da2118a331e045bf9c23882ddea0a172@sentry.io/125164"

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

                    ModeSelectionSceneState modeSelectionSceneState -> do
                        gamepads <- getGamepads

                        State state <- readRef ref >>= updateSound res.sounds

                        let modeMaybe = do
                                Gamepad gamepad <- join (gamepads !! 0)
                                GamepadButton button <- gamepad.buttons !! 0
                                Gamepad gamepad' <- join (state.gamepads !! 0)
                                GamepadButton button' <- gamepad'.buttons !! 0
                                guard $ not button'.pressed && button.pressed
                                let xs = [SinglePlayerMode, MultiplayerMode]
                                i <- findIndex (_ == modeSelectionSceneState.mode) xs
                                xs !! (mod (i + 1) (length xs))

                        case modeMaybe of
                            Nothing -> writeRef ref (State state { gamepads = gamepads })
                            Just mode -> runHalogenAff do
                                repaint driver $ State state {
                                    sceneState = ModeSelectionSceneState modeSelectionSceneState {
                                        mode =  mode
                                    },
                                    gamepads = gamepads
                                }

                    _ -> do
                        deltaTime <- getDeltaTime res.engine
                        readRef ref >>=
                            update deltaTime res driver >>=
                                updateBabylon deltaTime res >>=
                                    updateSound res.sounds >>=
                                        updatePhysics deltaTime playerBox world >>=
                                            writeRef ref
                        render res.scene






