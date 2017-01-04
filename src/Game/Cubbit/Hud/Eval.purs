module Game.Cubbit.Hud.Eval (eval, repaint, initializeTerrain) where

import Control.Alt (void)
import Control.Alternative (when)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef, writeRef)
import Control.Monad.Free (resume)
import Control.Monad.Rec.Class (Step(..), tailRecM2)
import DOM (DOM)
import DOM.Event.Event (preventDefault, stopPropagation)
import DOM.Event.KeyboardEvent (key, keyboardEventToEvent)
import DOM.Event.MouseEvent (MouseEvent, buttons)
import DOM.Event.WheelEvent (WheelEvent, wheelEventToEvent)
import Data.BooleanAlgebra (not)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (max, min)
import Data.Set (insert, delete)
import Data.Traversable (for_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.Aff (wait)
import Game.Cubbit.BlockType (airBlock)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Collesion (buildCollesionTerrain, updateChunkCollesion)
import Game.Cubbit.Config (Config(Config), writeConfig)
import Game.Cubbit.Control (pickBlock)
import Game.Cubbit.Hud.Start (start, clearTerrain, modifyAppState)
import Game.Cubbit.Hud.Type (HudEffects, PlayingSceneQuery(..), Query(..), QueryA(..), HudDriver, getRes)
import Game.Cubbit.MeshBuilder (editBlock, generateChunk)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.PointerLock (exitPointerLock, requestPointerLock)
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Terrain (Terrain(..), createTerrain, globalIndexToChunkIndex)
import Game.Cubbit.Types (GameMode(..), Mode(Move, Remove, Put), SceneState(PlayingSceneState, ModeSelectionSceneState, TitleSceneState), State(State))
import Graphics.Babylon.AbstractMesh (setReceiveShadows, setUseVertexColors)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.Scene (getDebugLayer, getMeshes)
import Graphics.Babylon.Sound (play, stop)
import Graphics.Babylon.Types (BABYLON)
import Graphics.Cannon (CANNON)
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (action)
import Math (pi)
import Prelude (type (~>), bind, negate, pure, ($), (*), (+), (-), (/=), (<$), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

eval :: forall eff. Ref State -> (Query ~> ComponentDSL State Query Void (Aff (HudEffects eff)))
eval ref query = case query of

    Query q next -> next <$ case q of

        (Repaint state') -> do
            liftEff $ writeRef ref state'
            put state'

        (SetActiveGameMode res mode) -> do
            modifyAppState ref (\(State state) -> State state {
                sceneState = case state.sceneState of
                    ModeSelectionSceneState s -> ModeSelectionSceneState s { mode = mode }
                    s -> s
            })

        (SetLanguage lang { sounds }) -> do
            liftEff $ play sounds.switchSound
            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    language = lang
                }
            })

        (PreventDefault e) -> liftEff do
            preventDefault e
            stopPropagation e

        (Nop e) -> liftEff do
            preventDefault e
            stopPropagation e

        (StopPropagation e) -> liftEff do
            stopPropagation e

        (ShowConfig { sounds }) -> do

            modifyAppState ref (\(State state) -> State state {
                configVisible = true
            })
            liftEff $ play sounds.switchSound

        (CloseConfig { sounds }) -> do

            modifyAppState ref (\(State state) -> State state {
                configVisible = false
            })
            liftEff $ play sounds.switchSound

        (SetBGMVolume { sounds } value) -> do

            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    bgmVolume = value
                }
            })
            liftEff do
                play sounds.switchSound
                State { config } <- readRef ref
                writeConfig config

        (SetSEVolume { sounds } value) -> do

            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    seVolume = value
                }
            })
            liftEff do
                play sounds.switchSound
                State { config } <- readRef ref
                writeConfig config

        (ToggleShadow { sounds }) -> do

            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    shadow = not config.shadow
                }
            })
            liftEff do


                play sounds.switchSound
                State state'@{ config: Config config } <- readRef ref
                writeConfig state'.config

                case getRes (State state') of
                    Nothing -> pure unit
                    Just res -> do
                        meshes <- getMeshes res.scene
                        for_ meshes $ setReceiveShadows config.shadow

        (ToggleVertexColor { sounds }) -> do

            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    vertexColor = not config.vertexColor
                }
            })
            liftEff do
                play sounds.switchSound
                State state' <- readRef ref
                writeConfig state'.config

                play sounds.switchSound
                State state''@{ config: Config config } <- readRef ref
                writeConfig state''.config

                case getRes (State state'') of
                    Nothing -> pure unit
                    Just res -> do
                        meshes <- getMeshes res.scene
                        for_ meshes $ setUseVertexColors config.vertexColor

        (ToggleWaterMaterial { sounds }) -> do
            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    waterMaterial = not config.waterMaterial
                }
            })
            liftEff do
                play sounds.switchSound
                State state' <- readRef ref
                writeConfig state'.config

        (SetShadowArea { sounds } value) -> do

            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    shadowArea = value
                }
            })
            liftEff do
                play sounds.switchSound
                State state' <- readRef ref
                writeConfig state'.config



        (SetChunkArea { sounds } value) -> do

            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    chunkArea = value
                }
            })
            liftEff do
                play sounds.switchSound
                State state' <- readRef ref
                writeConfig state'.config

        Home res -> do
            let nextScene = TitleSceneState {
                        res: res,
                        position: 0.0
                    }

            liftEff $ play res.sounds.warpSound
            modifyAppState ref (\(State state) -> State state { nextScene = true })
            wait 1000
            liftEff do
                stop res.sounds.forestSound
            modifyAppState ref (\(State state) -> State state {
                sceneState = nextScene,
                nextBGM = Just res.sounds.cleaning
            })
            wait 1000
            modifyAppState ref (\(State state) -> State state {
                nextScene = false
            })

        ModeSelect res -> do
            liftEff $ play res.sounds.warpSound
            modifyAppState ref (\(State state) -> State state {
                nextScene = true
            })
            wait 1000
            -- liftEff $ initializeTerrain ref
            modifyAppState ref (\(State state) -> State state {
                sceneState = ModeSelectionSceneState { res, mode: SinglePlayerMode },
                nextBGM = Just res.sounds.ichigo
            })
            wait 1000
            modifyAppState ref (\(State state) -> State state {
                nextScene = false
            })
            pure unit

        (Start gameMode res) -> do
            State state <- liftEff $ readRef ref
            start ref (State state) res gameMode

        (ToggleMute) -> do
            modifyAppState ref (\(State state@{ config: Config config }) -> State state {
                config = Config config {
                    mute = not config.mute
                }
            })

            liftEff do
                State state@{ config: Config config } <- readRef ref
                writeConfig state.config


        (PlayingSceneQuery playingSceneQuery) -> do

            -- todo
            State gameState <- liftEff $ readRef ref
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
                                    else exitPointerLock

                        (SetMousePosition e) -> liftEff do
                            modifyRef ref \(State state) ->
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


                        (OnMouseClick e) -> liftEff do

                            State state <- readRef ref

                            modifyRef ref \(State s) -> State s {
                                mousePosition = {
                                    x: offsetX e ,
                                    y: offsetY e
                                }
                            }

                            when (buttons e == 1) do

                                let put block = do
                                        picked <- pickBlock res.scene res.cursor playingSceneState.mode state.terrain state.mousePosition.x state.mousePosition.y
                                        for_ picked \blockIndex -> do
                                            editBlock ref blockIndex block res
                                            terrain' <- updateChunkCollesion state.terrain state.world (globalIndexToChunkIndex blockIndex)
                                            modifyRef ref \(State state) -> State state {
                                                terrain = terrain'
                                            }

                                case playingSceneState.mode of
                                    Put blockType -> do
                                        put blockType
                                        play res.sounds.putSound
                                    Remove -> do
                                        put airBlock
                                        play res.sounds.pickSound
                                    Move -> pure unit


                        (Zoom e) -> liftEff do

                            modifyRef ref \(State state) -> State state {
                                sceneState = PlayingSceneState playingSceneState {
                                    cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (playingSceneState.cameraRange + (toNumber (deltaY e) * options.wheelSpeed * options.cameraZoomSpeed)))
                                }
                            }
                            preventDefault (wheelEventToEvent e)
                            stopPropagation (wheelEventToEvent e)

                        (OnKeyDown e) -> liftEff do
                            modifyRef ref \(State state) -> State state { keys = insert (key e) state.keys }
                            case key e of
                                "1" -> do
                                    modifyRef ref \(State state) -> State state { debugLayer = not state.debugLayer }
                                    State state <- readRef ref
                                    if state.debugLayer
                                        then getDebugLayer res.scene >>= DebugLayer.show true true Nothing
                                        else getDebugLayer res.scene >>= DebugLayer.hide
                                "2" -> openDevTools
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


                otherSceneState -> pure unit







repaint :: forall eff. HudDriver eff -> State -> Aff (HudEffects eff) Unit
repaint driver state = driver.query $ action $ Query $ Repaint state

offsetX :: MouseEvent -> Int
offsetX e = (unsafeCoerce e).offsetX

offsetY :: MouseEvent -> Int
offsetY e = (unsafeCoerce e).offsetY

deltaY :: WheelEvent -> Int
deltaY e = (unsafeCoerce e).deltaY





initializeTerrain :: forall eff. Ref State -> Resources -> Eff (babylon :: BABYLON, cannon :: CANNON, ref :: REF | eff) Unit
initializeTerrain ref res@{ options: Options options } = do

    State state@{ terrain: Terrain terrain } <- readRef ref


    clearTerrain (Terrain terrain) state.world

    -- update state
    emptyTerrain <- createTerrain 0
    writeRef ref $ State state {
        terrain = emptyTerrain
    }

    -- initialize chunk and mesh
    do
        let initialWorldSize = options.initialWorldSize
        forE (-initialWorldSize) initialWorldSize \x -> do
            forE (-initialWorldSize) initialWorldSize \z -> void do
                let index = chunkIndex x 0 z
                State s <- readRef ref
                generateChunk (State s) res.materials res.scene index res.options s.config res

    -- initialize cannon world
    terrain' <- tailRecM2 (\ter -> case _ of
        0 -> pure $ Done ter
        i -> do
            ter' <- buildCollesionTerrain ter state.world (chunkIndex 0 0 0)
            pure $ Loop { a: ter', b: i - 1 }
    ) emptyTerrain 9
    modifyRef ref \(State s) -> State s { terrain = terrain' }





foreign import openDevTools :: forall eff. Eff (dom :: DOM | eff) Unit

