module Game.Cubbit.Hud.Eval (eval, repaint) where

import Control.Alternative (when)
import Control.Bind (join)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef)
import Control.Monad.RWS (modify)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM2)
import Control.MonadPlus (guard)
import DOM (DOM)
import DOM.Event.Event (preventDefault, stopPropagation)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.KeyboardEvent (key, keyboardEventToEvent)
import DOM.Event.MouseEvent (MouseEvent, buttons)
import DOM.Event.WheelEvent (WheelEvent, wheelEventToEvent)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (resize) as DOM
import DOM.HTML.HTMLElement (focus)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..))
import Data.Array (findIndex, length, (!!), (..))
import Data.BooleanAlgebra (not)
import Data.EuclideanRing (mod)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Ord (max, min)
import Data.Set (insert, delete)
import Data.Traversable (for_, traverse_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.Aff (wait)
import Game.Cubbit.BlockType (airBlock)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Collesion (buildCollesionTerrain, createPlayerCollesion, updateChunkCollesion, updatePhysics)
import Game.Cubbit.Config (Config(Config), writeConfig)
import Game.Cubbit.Control (pickBlock)
import Game.Cubbit.Hud.Start (start, clearTerrain, modifyAppState)
import Game.Cubbit.Hud.Type (HudEffects, PlayingSceneQuery(..), Query(..), QueryA(..), HudDriver, getRes)
import Game.Cubbit.MeshBuilder (editBlock, generateChunk)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (Resources, loadResourcesH)
import Game.Cubbit.Terrain (Terrain(..), createTerrain, globalIndexToChunkIndex)
import Game.Cubbit.Types (GameMode(..), Mode(Move, Remove, Put), SceneState(..), State(State))
import Game.Cubbit.Update (updateBabylon, updateH, updateSound)
import Gamepad (Gamepad(..), GamepadButton(..), getGamepads)
import Graphics.Babylon.AbstractMesh (setReceiveShadows, setUseVertexColors)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.Engine (getDeltaTime, resize, runRenderLoop)
import Graphics.Babylon.Scene (getDebugLayer, getMeshes, render)
import Graphics.Babylon.Sound (play, stop)
import Graphics.Babylon.Util (querySelectorCanvasAff)
import Graphics.Cannon (addBody, setGravity)
import Graphics.Cannon.Vec3 (createVec3)
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (action, get, lift)
import Math (pi)
import PointerLock (exitPointerLock, requestPointerLock)
import Prelude (type (~>), bind, negate, pure, ($), (&&), (*), (+), (-), (/=), (<#>), (<$), (<$>), (<<<), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

eval :: forall eff. Ref State -> (Query ~> ComponentDSL State Query Void (Aff (HudEffects eff)))
eval ref query = case query of

    Query q next -> next <$ case q of

        Gameloop -> do

            let inc = modify \(State state) -> State state {
                        sceneState = case state.sceneState of
                            LoadingSceneState count -> LoadingSceneState $ count + 1
                            x -> x
                    }

            canvasGL <- liftAff $ querySelectorCanvasAff "#renderCanvas"

            res@{ options: Options options } <- loadResourcesH canvasGL inc


            modify \(State state) -> State state {
                nextBGM = Just res.sounds.cleaning,
                sceneState = TitleSceneState {
                    res: res,
                    position: 0.0
                }
            }

            -- initialize cannon --
            State state <- get


            playerBox <- liftEff do
                gravity <- createVec3 0.0 options.gravity 0.0
                setGravity gravity state.world
                pBox <- createPlayerCollesion
                addBody pBox state.world
                pure pBox

            -- clear terrain mesh and terrain bodies
            initializeTerrain res

            liftEff do
                -- focus the element
                (window >>= document <#> htmlDocumentToNonElementParentNode >>= getElementById (ElementId "content") <#> toMaybe) >>= traverse_ (focus <<< unsafeCoerce)

                -- add resize event listener
                (windowToEventTarget <$> window) >>= addEventListener DOM.resize (eventListener $ \_ -> resize res.engine) false


            -- ********************************************************
            -- **HACK** runRenderLoop invoke the effect repeatedly!!
            -- *************************************************************
            lift $ makeAff \reject resolve -> runRenderLoop (resolve unit) res.engine
            State st <- get
            case st.sceneState of
                ModeSelectionSceneState modeSelectionSceneState -> do
                    gamepads <- liftEff $ getGamepads

                    liftEff $ updateSound res.sounds (State st)

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
                        Nothing -> put (State st { gamepads = gamepads })
                        Just mode -> put $ State state {
                            sceneState = ModeSelectionSceneState modeSelectionSceneState {
                                mode =  mode
                            },
                            gamepads = gamepads
                        }

                _ -> do
                    st' <- liftEff do
                        deltaTime <- getDeltaTime res.engine

                        st'' <- pure (State st) >>=
                            updateH deltaTime res >>=
                                updateBabylon deltaTime res >>=
                                    updateSound res.sounds >>=
                                        updatePhysics deltaTime playerBox state.world

                        render res.scene

                        pure st''

                    put st'



            pure unit

        (Repaint state') -> put state'

        (SetActiveGameMode res mode) -> do
            modify \(State state) -> State state {
                sceneState = case state.sceneState of
                    ModeSelectionSceneState s -> ModeSelectionSceneState s { mode = mode }
                    s -> s
            }

        (SetLanguage lang { sounds }) -> do
            liftEff $ play sounds.switchSound
            modify \(State state@{ config: Config config }) -> State state {
                config = Config config {
                    language = lang
                }
            }

        (PreventDefault e) -> liftEff do
            preventDefault e
            stopPropagation e

        (Nop e) -> liftEff do
            preventDefault e
            stopPropagation e

        (StopPropagation e) -> liftEff do
            stopPropagation e

        (ShowConfig { sounds }) -> do

            modify \(State state) -> State state {
                configVisible = true
            }
            liftEff $ play sounds.switchSound

        (CloseConfig { sounds }) -> do

            modify \(State state) -> State state {
                configVisible = false
            }
            liftEff $ play sounds.switchSound

        (SetBGMVolume { sounds } value) -> do

            modify \(State state@{ config: Config config }) -> State state {
                config = Config config {
                    bgmVolume = value
                }
            }

            State { config } <- get
            liftEff do
                play sounds.switchSound
                writeConfig config

        (SetSEVolume { sounds } value) -> do

            modify \(State state@{ config: Config config }) -> State state {
                config = Config config {
                    seVolume = value
                }
            }

            State { config } <- get
            liftEff do
                play sounds.switchSound
                writeConfig config

        (ToggleShadow { sounds }) -> do

            modify \(State state@{ config: Config config }) -> State state {
                config = Config config {
                    shadow = not config.shadow
                }
            }

            State state'@{ config: Config config } <- get
            liftEff do
                play sounds.switchSound
                writeConfig state'.config
                case getRes (State state') of
                    Nothing -> pure unit
                    Just res -> do
                        meshes <- getMeshes res.scene
                        for_ meshes $ setReceiveShadows config.shadow

        (ToggleVertexColor { sounds }) -> do

            modify \(State state@{ config: Config config }) -> State state {
                config = Config config {
                    vertexColor = not config.vertexColor
                }
            }

            State state'@{ config: Config config } <- get

            liftEff do
                play sounds.switchSound
                writeConfig state'.config

                case getRes (State state') of
                    Nothing -> pure unit
                    Just res -> do
                        meshes <- getMeshes res.scene
                        for_ meshes $ setUseVertexColors config.vertexColor

        (ToggleWaterMaterial { sounds }) -> do
            modify \(State state@{ config: Config config }) -> State state {
                config = Config config {
                    waterMaterial = not config.waterMaterial
                }
            }
            State state' <- get
            liftEff do
                play sounds.switchSound
                writeConfig state'.config

        (SetShadowArea { sounds } value) -> do

            modify \(State state@{ config: Config config }) -> State state {
                config = Config config {
                    shadowArea = value
                }
            }
            State state' <- get
            liftEff do
                play sounds.switchSound
                writeConfig state'.config

        (SetChunkArea { sounds } value) -> do

            modify \(State state@{ config: Config config }) -> State state {
                config = Config config {
                    chunkArea = value
                }
            }
            State state' <- get
            liftEff do
                play sounds.switchSound
                writeConfig state'.config

        Home res -> do
            let nextScene = TitleSceneState {
                        res: res,
                        position: 0.0
                    }

            liftEff $ play res.sounds.warpSound
            modify \(State state) -> State state { nextScene = true }
            wait 1000
            liftEff do
                stop res.sounds.forestSound
            modify \(State state) -> State state {
                sceneState = nextScene,
                nextBGM = Just res.sounds.cleaning
            }
            wait 1000
            modify \(State state) -> State state {
                nextScene = false
            }

        ModeSelect res -> do
            liftEff $ play res.sounds.warpSound
            modify \(State state) -> State state {
                nextScene = true
            }
            wait 1000
            -- liftEff $ initializeTerrain ref
            modify \(State state) -> State state {
                sceneState = ModeSelectionSceneState { res, mode: SinglePlayerMode },
                nextBGM = Just res.sounds.ichigo
            }
            wait 1000
            modify \(State state) -> State state {
                nextScene = false
            }
            pure unit

        (Start gameMode res) -> do
            State state <- get
            start ref (State state) res gameMode

        (ToggleMute) -> do
            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    mute = not config.mute
                }
            })

            State state@{ config: Config config } <- get
            liftEff do
                writeConfig state.config

        (PlayingSceneQuery playingSceneQuery) -> do

            -- todo
            State gameState <- get
            case gameState.sceneState of

                PlayingSceneState playingSceneState@{ res: res@{ options: Options options }} -> do

                    case playingSceneQuery of

                        (SetCursorPosition position) -> do

                            when (position /= playingSceneState.cursorPosition) do
                                modifyAppState ref (\(State state) -> State state {
                                    sceneState = PlayingSceneState playingSceneState {
                                        cursorPosition = position
                                    }
                                })

                        (SetMode mode) -> do

                            liftEff do
                                when (playingSceneState.mode /= mode) do
                                    play res.sounds.switchSound

                            modify \(State state) -> State state {
                                sceneState = PlayingSceneState playingSceneState {
                                    mode = mode
                                }
                            }

                        (SetPosition position) -> do
                            modify \(State s) -> State s {
                                sceneState = PlayingSceneState playingSceneState {
                                    position = position
                                }
                            }

                        TogglePointerLock -> do
                            pure unit
                            {-
                                let firstPersonView = not playingSceneState.firstPersonView

                                modify \(State state) -> State state {
                                    sceneState = PlayingSceneState playingSceneState {
                                        firstPersonView = firstPersonView
                                    }
                                }


                                if firstPersonView
                                    then liftEff $ requestPointerLock (\e -> do
                                        modifyRef ref (\(State state) -> State state {
                                            sceneState = case state.sceneState of
                                                PlayingSceneState ps -> PlayingSceneState ps {
                                                    playerRotation = ps.playerRotation + e.movementX * options.pointerHorizontalSensitivity,
                                                    playerPitch = max (-pi * 0.45) (min (pi * 0.45) ps.playerPitch - e.movementY * options.pointerVerticalSensitivity)
                                                }
                                                s -> s
                                        })
                                        pure unit
                                    ) (modifyRef ref (\(State state) -> State state {
                                        sceneState = case state.sceneState of
                                            PlayingSceneState ps -> PlayingSceneState ps {
                                                firstPersonView = false
                                            }
                                            s -> s
                                    }))
                                    else liftEff exitPointerLock
                                    -}

                        (SetMousePosition e) -> do
                            modify \(State state) ->
                                let isRightButton = buttons e == 2
                                    dx = offsetX e - state.mousePosition.x
                                    dy = offsetY e - state.mousePosition.y
                                in State state {
                                        mousePosition = {
                                            x: offsetX e ,
                                            y: offsetY e
                                        },
                                        sceneState = case state.sceneState of
                                            PlayingSceneState p -> PlayingSceneState p {
                                                cameraYaw = if isRightButton then playingSceneState.cameraYaw + toNumber dx * options.cameraHorizontalSensitivity else playingSceneState.cameraYaw,
                                                cameraPitch = if isRightButton then max (-pi * 0.45) $ min (pi * 0.45) $ playingSceneState.cameraPitch + toNumber dy * options.cameraVertialSensitivity else playingSceneState.cameraPitch
                                            }
                                            s -> s
                                    }


                        (OnMouseClick e) -> do
                            State state <- get

                            modify \(State s) -> State s {
                                mousePosition = {
                                    x: offsetX e ,
                                    y: offsetY e
                                }
                            }

                            when (buttons e == 1) do

                                let put block = do
                                        picked <- liftEff $ pickBlock res.scene res.cursor playingSceneState.mode state.terrain state.mousePosition.x state.mousePosition.y
                                        for_ picked \blockIndex -> do
                                            State s <- get
                                            liftEff $ editBlock (State s) blockIndex block res
                                            terrain' <- liftEff $ updateChunkCollesion state.terrain state.world (globalIndexToChunkIndex blockIndex)
                                            modify \(State state) -> State state {
                                                terrain = terrain'
                                            }

                                case playingSceneState.mode of
                                    Put blockType -> do
                                        put blockType
                                        liftEff $ play res.sounds.putSound
                                    Remove -> do
                                        put airBlock
                                        liftEff $ play res.sounds.pickSound
                                    Move -> pure unit


                        (Zoom e) -> do

                            modify \(State state) -> State state {
                                sceneState = PlayingSceneState playingSceneState {
                                    cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (playingSceneState.cameraRange + (toNumber (deltaY e) * options.wheelSpeed * options.cameraZoomSpeed)))
                                }
                            }

                            liftEff do
                                preventDefault (wheelEventToEvent e)
                                stopPropagation (wheelEventToEvent e)

                        (OnKeyDown e) -> do
                            modify \(State state) -> State state { keys = insert (key e) state.keys }
                            case key e of
                                "1" -> do
                                    modify \(State state) -> State state { debugLayer = not state.debugLayer }
                                    State state <- get
                                    liftEff if state.debugLayer
                                        then getDebugLayer res.scene >>= DebugLayer.show true true Nothing
                                        else getDebugLayer res.scene >>= DebugLayer.hide
                                "2" -> liftEff openDevTools
                                _ -> pure unit
                            liftEff do
                                preventDefault (keyboardEventToEvent e)
                                stopPropagation (keyboardEventToEvent e)

                        (OnKeyUp e) -> do
                            modify \(State state) -> State state { keys = delete (key e) state.keys }
                            liftEff do
                                preventDefault (keyboardEventToEvent e)
                                stopPropagation (keyboardEventToEvent e)

                        (SetCenterPanelVisible visible) -> do

                            modify \(State state) -> State state {
                                sceneState = PlayingSceneState playingSceneState { centerPanelVisible = visible }
                            }
                            pure unit


                otherSceneState -> pure unit







repaint :: forall eff. HudDriver eff -> State -> Aff (HudEffects eff) Unit
repaint driver state = driver.query $ action $ Query $ Repaint state

offsetX :: MouseEvent -> Int
offsetX e = (unsafeCoerce e).offsetX

offsetY :: MouseEvent -> Int
offsetY e = (unsafeCoerce e).offsetY

deltaY :: WheelEvent -> Int
deltaY e = (unsafeCoerce e).deltaY





initializeTerrain :: forall eff. Resources -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
initializeTerrain res@{ options: Options options } = do
    State state@{ terrain: Terrain terrain } <- get

    liftEff $ clearTerrain (Terrain terrain) state.world

    -- update state
    emptyTerrain <- liftEff $ createTerrain 0
    put $ State state {
        terrain = emptyTerrain
    }

        -- initialize chunk and mesh
    let initialWorldSize = options.initialWorldSize
    for_ ((-initialWorldSize) .. initialWorldSize) \x -> do
        for_ ((-initialWorldSize) .. initialWorldSize) \z -> do
            let index = chunkIndex x 0 z
            State s <- get
            liftEff $ generateChunk (State s) res.materials res.scene index res.options s.config res

    -- initialize cannon world
    terrain' <- tailRecM2 (\ter -> case _ of
        0 -> pure $ Done ter
        i -> do
            ter' <- liftEff $ buildCollesionTerrain ter state.world (chunkIndex 0 0 0)
            pure $ Loop { a: ter', b: i - 1 }
    ) emptyTerrain 9

    modify $ \(State s) -> State s { terrain = terrain' }

foreign import openDevTools :: forall eff. Eff (dom :: DOM | eff) Unit

