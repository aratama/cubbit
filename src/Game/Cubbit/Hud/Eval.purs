module Game.Cubbit.Hud.Eval (eval, repaint) where

import Control.Alt (void)
import Control.Alternative (when)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef)
import DOM.Event.Event (preventDefault, stopPropagation)
import DOM.Event.KeyboardEvent (key, keyboardEventToEvent)
import DOM.Event.MouseEvent (MouseEvent, buttons)
import DOM.Event.WheelEvent (WheelEvent, wheelEventToEvent)
import Data.BooleanAlgebra (not)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Ord (max, min)
import Data.Set (insert, delete)
import Data.Traversable (for_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.Aff (wait)
import Game.Cubbit.BlockIndex (blockIndex)
import Game.Cubbit.BlockType (airBlock)
import Game.Cubbit.Config (Config(Config), writeConfig)
import Game.Cubbit.Control (pickBlock)
import Game.Cubbit.Hud.Type (HudEffects, PlayingSceneQuery(..), Query(..), QueryA(..), HudDriver)
import Game.Cubbit.MeshBuilder (editBlock)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.PointerLock (exitPointerLock, requestPointerLock)
import Game.Cubbit.Sounds (playBGM, setBGMVolume, setMute, setSEVolume, stopBGM)
import Game.Cubbit.Types (Mode(..), SceneState(..), State(..), ResourceProgress(..))
import Graphics.Babylon.AbstractMesh (setIsVisible)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.Scene (getDebugLayer)
import Graphics.Babylon.Sound (play, stop)
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (action)
import Math (pi)
import Prelude (type (~>), bind, negate, pure, ($), (*), (+), (-), (/=), (==), (>>=), (/), (<$))
import Unsafe.Coerce (unsafeCoerce)

eval :: forall eff. Ref State -> (Query ~> ComponentDSL State Query Void (Aff (HudEffects eff)))
eval ref query = do

    State currentState <- liftEff $ readRef ref
    case currentState.res of

        Loading progress -> case query of

            Query q next -> next <$ case q of
                (Repaint state) -> do
                    liftEff $ writeRef ref state
                    put state
                _ -> pure unit

        Complete res -> do
            let cursor = res.cursor
            let sounds = res.sounds
            let materials = res.materials
            let scene = res.scene
            let playerMeshes = res.playerMeshes
            Options options <- pure res.options


            case query of
                Query q next -> next <$ case q of


                    (Repaint state') -> do
                        liftEff $ writeRef ref state'
                        put state'

                    (PreventDefault e) -> do
                        liftEff (preventDefault e)
                        liftEff (stopPropagation e)

                    (Nop e) -> do
                        liftEff do
                            preventDefault e
                            stopPropagation e

                    (StopPropagation e) -> do
                        liftEff $ stopPropagation e

                    (ShowConfig) -> do

                        modifyAppState ref (\(State state) -> State state {
                            configVisible = true
                        })
                        liftEff $ play sounds.switchSound

                    (CloseConfig) -> do

                        modifyAppState ref (\(State state) -> State state {
                            configVisible = false
                        })
                        liftEff $ play sounds.switchSound

                    (SetBGMVolume value) -> do

                        modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                            config = Config config {
                                bgmVolume = value
                            }
                        })
                        liftEff do
                            play sounds.switchSound
                            setBGMVolume (toNumber value / 8.0) sounds
                            State state' <- readRef ref
                            writeConfig state'.config

                    (SetSEVolume value) -> do


                        modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                            config = Config config {
                                seVolume = value
                            }
                        })
                        liftEff do
                            play sounds.switchSound
                            setSEVolume (toNumber value / 8.0) sounds
                            State state' <- readRef ref
                            writeConfig state'.config

                    (ToggleShadow) -> do

                        modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                            config = Config config {
                                shadow = not config.shadow
                            }
                        })
                        liftEff do
                            play sounds.switchSound
                            State state' <- readRef ref
                            writeConfig state'.config

                    (ToggleVertexColor) -> do

                        modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                            config = Config config {
                                vertexColor = not config.vertexColor
                            }
                        })
                        liftEff do
                            play sounds.switchSound
                            State state' <- readRef ref
                            writeConfig state'.config

                    (SetShadowArea value) -> do

                        modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                            config = Config config {
                                shadowArea = value
                            }
                        })
                        liftEff do
                            play sounds.switchSound
                            State state' <- readRef ref
                            writeConfig state'.config



                    (SetChunkArea value) -> do

                        modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                            config = Config config {
                                chunkArea = value
                            }
                        })
                        liftEff do
                            play sounds.switchSound
                            State state' <- readRef ref
                            writeConfig state'.config


                    (Start) -> do

                        let nextScene = PlayingSceneState {
                                    cameraYaw: 0.0,
                                    cameraPitch: 0.7,
                                    cameraRange: 5.0,
                                    firstPersonView: false,
                                    firstPersonViewPitch: 0.0,
                                    position: { x: 0.5, y: 10.0, z: 0.5 },
                                    velocity: { x: 0.0, y: 0.0, z: 0.0 },
                                    playerRotation: 0.5,
                                    playerPitch: 0.0,
                                    animation: "",
                                    mode: Move,
                                    landing: 0,

                                    cursorPosition: blockIndex 0 0 0,
                                    centerPanelVisible: false,
                                    life: 10,
                                    maxLife: 12
                                }

                        liftEff $ play sounds.warpSound
                        modifyAppState ref (\(State state) -> State state { nextScene = Just nextScene })
                        wait 1000
                        modifyAppState ref (\(State state) -> State state {
                            cameraPosition = { x: 10.0, y: 20.0, z: negate 10.0 },
                            cameraTarget = { x: 0.5, y: 11.0, z: 0.5 },
                            sceneState = nextScene
                        })
                        liftEff do
                            State state@{ config: Config config } <- readRef ref
                            stopBGM config.bgmVolume sounds

                            for_ playerMeshes \mesh -> void do
                                setIsVisible true mesh
                        wait 1000
                        liftEff do
                            play sounds.forestSound
                            State { config: Config config } <- readRef ref
                            playBGM sounds.rye config.bgmVolume sounds
                        modifyAppState ref (\(State state) -> State state { nextScene = Nothing })




                    (ToggleMute) -> do
                        modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                            config = Config config {
                                mute = not config.mute
                            }
                        })

                        liftEff do
                            State state@{ config: Config config } <- readRef ref
                            setMute config.mute sounds
                            writeConfig state.config


                    (PlayingSceneQuery playingSceneQuery) -> do

                        -- todo
                        State gameState <- liftEff $ readRef ref
                        case gameState.sceneState of
                            TitleSceneState titleSceneState -> pure unit




                            PlayingSceneState playingSceneState -> do

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
                                                play sounds.switchSound

                                        modifyAppState ref (\(State state) -> State state {
                                            sceneState = PlayingSceneState playingSceneState {
                                                mode = mode
                                            }
                                        })

                                        pure unit
                                    (SetPosition position) -> do

                                        liftEff (modifyRef ref (\(State s) -> State s {
                                            sceneState = PlayingSceneState playingSceneState {
                                                position = position
                                            }
                                        }))

                                    TogglePointerLock -> do
                                        liftEff do
                                            let firstPersonView = not playingSceneState.firstPersonView
                                            modifyRef ref (\(State state) -> State state {
                                                sceneState = PlayingSceneState playingSceneState {
                                                    firstPersonView = firstPersonView
                                                }
                                            })
                                            if firstPersonView
                                                then requestPointerLock (\e -> do
                                                    modifyRef ref (\(State state) -> State state {
                                                        sceneState = case state.sceneState of
                                                            TitleSceneState ts -> TitleSceneState ts
                                                            PlayingSceneState ps -> PlayingSceneState ps {
                                                                playerRotation = ps.playerRotation + e.movementX * options.pointerHorizontalSensitivity,
                                                                playerPitch = max (-pi * 0.45) (min (pi * 0.45) ps.playerPitch - e.movementY * options.pointerVerticalSensitivity)
                                                            }
                                                    })
                                                    pure unit
                                                ) (modifyRef ref (\(State state) -> State state {
                                                    sceneState = case state.sceneState of
                                                        TitleSceneState ts -> TitleSceneState ts
                                                        PlayingSceneState ps -> PlayingSceneState ps {
                                                            firstPersonView = false
                                                        }
                                                }))
                                                else exitPointerLock

                                    (SetMousePosition e) -> do
                                        liftEff do
                                            modifyRef ref \(State state) ->
                                                let isRightButton = buttons e == 2
                                                    dx = offsetX e - state.mousePosition.x
                                                    dy = offsetY e - state.mousePosition.y
                                                        in State state {
                                                                mousePosition = {
                                                                    x: offsetX e ,
                                                                    y: offsetY e
                                                                },
                                                                sceneState = PlayingSceneState playingSceneState {
                                                                    cameraYaw = if isRightButton then playingSceneState.cameraYaw + toNumber dx * options.cameraHorizontalSensitivity else playingSceneState.cameraYaw,
                                                                    cameraPitch = if isRightButton then max (-pi * 0.45) $ min (pi * 0.45) $ playingSceneState.cameraPitch + toNumber dy * options.cameraVertialSensitivity else playingSceneState.cameraPitch
                                                                }
                                                            }




                                    (OnMouseClick e) -> do
                                        liftEff do

                                            State state <- readRef ref

                                            modifyRef ref \(State s) -> State s {
                                                mousePosition = {
                                                    x: offsetX e ,
                                                    y: offsetY e
                                                }
                                            }

                                            when (buttons e == 1) do

                                                let put block = do
                                                        picked <- pickBlock scene cursor playingSceneState.mode state.terrain state.mousePosition.x state.mousePosition.y
                                                        case picked of
                                                            Nothing -> pure unit
                                                            Just blockIndex -> editBlock ref blockIndex block

                                                case playingSceneState.mode of
                                                    Put blockType -> do
                                                        put blockType
                                                        play sounds.putSound
                                                    Remove -> do
                                                        put airBlock
                                                        play sounds.pickSound
                                                    Move -> pure unit


                                    (Zoom e) -> do
                                        liftEff do
                                            modifyRef ref \(State state) -> State state {
                                                sceneState = PlayingSceneState playingSceneState {
                                                    cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (playingSceneState.cameraRange + (toNumber (deltaY e) * options.cameraZoomSpeed)))
                                                }
                                            }
                                            preventDefault (wheelEventToEvent e)
                                            stopPropagation (wheelEventToEvent e)


                                    (OnKeyDown e) -> liftEff do
                                        modifyRef ref \(State state) -> State state { keys = insert (key e) state.keys }
                                        case key e of
                                            "1" -> do
                                                State state <- readRef ref
                                                if state.debugLayer
                                                    then getDebugLayer scene >>= DebugLayer.show true true Nothing
                                                    else getDebugLayer scene >>= DebugLayer.hide
                                            _ -> pure unit
                                        preventDefault (keyboardEventToEvent e)
                                        stopPropagation (keyboardEventToEvent e)

                                    (OnKeyUp e) -> liftEff do
                                        modifyRef ref \(State state) -> State state { keys = delete (key e) state.keys }
                                        preventDefault (keyboardEventToEvent e)
                                        stopPropagation (keyboardEventToEvent e)

                                    (SetCenterPanelVisible visible) -> do

                                        modifyAppState ref \(State state) -> State state {
                                            sceneState = PlayingSceneState playingSceneState { centerPanelVisible = visible }
                                        }
                                        pure unit



                                    Home -> do
                                        let nextScene = TitleSceneState {
                                                    position: 0.0
                                                }

                                        liftEff $ play sounds.warpSound
                                        modifyAppState ref (\(State state) -> State state { nextScene = Just nextScene })
                                        wait 1000
                                        liftEff do
                                            State { config: Config config } <- readRef ref
                                            stopBGM config.bgmVolume sounds
                                            stop sounds.forestSound
                                        modifyAppState ref (\(State state) -> State state { sceneState = nextScene })
                                        wait 1000
                                        liftEff do
                                            State { config: Config config } <- readRef ref
                                            playBGM sounds.cleaning config.bgmVolume sounds
                                        modifyAppState ref (\(State state) -> State state { nextScene = Nothing })



modifyAppState :: forall eff. Ref State -> (State -> State) -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
modifyAppState ref f = do
    state <- liftEff $ readRef ref
    let state' = (f state)
    liftEff $ writeRef ref state'
    put state'



repaint :: forall eff. HudDriver eff -> State -> Aff (HudEffects eff) Unit
repaint driver state = driver.query $ action $ Query $ Repaint state

offsetX :: MouseEvent -> Int
offsetX e = (unsafeCoerce e).offsetX

offsetY :: MouseEvent -> Int
offsetY e = (unsafeCoerce e).offsetY

deltaY :: WheelEvent -> Int
deltaY e = (unsafeCoerce e).deltaY

