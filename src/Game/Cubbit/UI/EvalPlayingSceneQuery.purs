module Game.Cubbit.Hud.EvalPlayingSceneQuery (evalPlayingSceneQuery) where

import Control.Alternative (when)
import Control.Monad.Aff (Aff)
import Control.Monad.RWS (modify)
import DOM.Event.Event (preventDefault, stopPropagation)
import DOM.Event.KeyboardEvent (key, keyboardEventToEvent)
import DOM.Event.MouseEvent (MouseEvent, buttons)
import DOM.Event.WheelEvent (WheelEvent, wheelEventToEvent)
import Data.BooleanAlgebra (not)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (max, min)
import Data.Set (insert, delete)
import Data.Traversable (for_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockType (airBlock)
import Game.Cubbit.Collesion (updateChunkCollesion)
import Game.Cubbit.Control (pickBlock)
import Game.Cubbit.Hud.DevTools (openDevTools)
import Game.Cubbit.Hud.Type (HudEffects, PlayingSceneQuery(..), Query)
import Game.Cubbit.MeshBuilder (editBlock)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Terrain (globalIndexToChunkIndex)
import Game.Cubbit.Types (Mode(Move, Remove, Put), SceneState(PlayingSceneState), State(State))
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.Scene (getDebugLayer)
import Graphics.Babylon.Sound (play)
import Halogen (ComponentDSL, liftEff)
import Halogen.Query (get)
import Math (pi)
import PointerLock (exitPointerLock, requestPointerLock)
import Prelude (bind, negate, pure, ($), (*), (+), (-), (/=), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

evalPlayingSceneQuery :: forall eff. PlayingSceneQuery -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
evalPlayingSceneQuery playingSceneQuery = do

    -- todo
    State gameState <- get
    case gameState.sceneState of

        PlayingSceneState playingSceneState@{ res: res@{ options: Options options }} -> do

            case playingSceneQuery of

                (SetCursorPosition position) -> do

                    when (position /= playingSceneState.cursorPosition) do
                        modify \(State state) -> State state {
                            sceneState = PlayingSceneState playingSceneState {
                                cursorPosition = position
                            }
                        }

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
                    liftEff if playingSceneState.firstPersonView then exitPointerLock else requestPointerLock

                (OnChangePointerlock pointerLockElement) -> do
                    modify \(State state) -> State state {
                        sceneState = case state.sceneState of
                            PlayingSceneState ps -> PlayingSceneState ps {
                                firstPersonView = isJust pointerLockElement
                            }
                            s -> s
                    }

                (OnMovePointer e) -> do
                    modify \(State state) -> State state {
                        sceneState = case state.sceneState of
                            PlayingSceneState ps -> PlayingSceneState ps {
                                playerRotation = ps.playerRotation + e.movementX * options.pointerHorizontalSensitivity,
                                playerPitch = max (-pi * 0.45) (min (pi * 0.45) ps.playerPitch - e.movementY * options.pointerVerticalSensitivity)
                            }
                            s -> s
                    }

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
                                    terrain' <- liftEff $ updateChunkCollesion s.terrain res.world (globalIndexToChunkIndex blockIndex)
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


offsetX :: MouseEvent -> Int
offsetX e = (unsafeCoerce e).offsetX

offsetY :: MouseEvent -> Int
offsetY e = (unsafeCoerce e).offsetY

deltaY :: WheelEvent -> Int
deltaY e = (unsafeCoerce e).deltaY


