module Game.Cubbit.Hud.Eval (eval) where

import Control.Alt (void)
import Control.Alternative (when)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import DOM.Event.Event (preventDefault, stopPropagation)
import DOM.Event.KeyboardEvent (key, keyboardEventToEvent)
import DOM.Event.MouseEvent (MouseEvent, buttons)
import Data.BooleanAlgebra (not)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Ord (max, min)
import Data.Traversable (for_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockIndex (blockIndex)
import Game.Cubbit.BlockType (airBlock)
import Game.Cubbit.Config (Config(Config), writeConfig)
import Game.Cubbit.Control (pickBlock)
import Game.Cubbit.Hud.Type (Query(..), HudEffects, PlayingSceneQuery(..))
import Game.Cubbit.Materials (Materials)
import Game.Cubbit.MeshBuilder (editBlock)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.PointerLock (exitPointerLock, requestPointerLock)
import Game.Cubbit.Sounds (Sounds, setMute, stopAllSounds)
import Game.Cubbit.Types (Mode(..), SceneState(..), State(..))
import Graphics.Babylon.AbstractMesh (setIsVisible)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.Scene (getDebugLayer)
import Graphics.Babylon.Sound (play, stop)
import Graphics.Babylon.Types (Mesh, Scene)
import Halogen (ComponentDSL, liftAff, liftEff, put)
import Halogen.Query (get)
import Math (pi)
import Prelude (type (~>), bind, negate, pure, ($), (*), (+), (-), (/=), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

eval :: forall eff. Scene -> Mesh -> Materials -> Options -> Ref State -> Sounds -> (Query ~> ComponentDSL State Query Void (Aff (HudEffects eff)))
eval scene cursor materials (Options options) ref sounds query = case query of

    (PeekState f) -> do
        s <- get
        pure (f s)

    (PreventDefault e next) -> do
        liftEff (preventDefault e)
        liftEff (stopPropagation e)
        pure next

    (Nop e next) -> do
        liftEff do
            preventDefault e
            stopPropagation e
        pure next

    (Start next) -> do

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
            stopAllSounds sounds
            State state <- readRef ref
            for_ state.playerMeshes \mesh -> void do
                setIsVisible true mesh
        wait 1000
        liftEff do
            play sounds.forestSound
            play sounds.rye
        modifyAppState ref (\(State state) -> State state { nextScene = Nothing })



        pure next


    (ToggleMute next) -> do
        modifyAppState ref (\(State state@{ config: Config config }) -> State state {
            config = Config config {
                mute = not config.mute
            }
        })

        liftEff do
            State state@{ config: Config config } <- readRef ref
            setMute config.mute sounds
            writeConfig state.config


        pure next

    (PlayingSceneQuery playingSceneQuery next) -> do

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


                    (ToggleDebugLayer) -> do
                        liftEff do
                            modifyRef ref (\(State state) -> State state { debugLayer = not state.debugLayer })

                            State state <- readRef ref
                            if state.debugLayer
                                then getDebugLayer scene >>= DebugLayer.show true true Nothing
                                else getDebugLayer scene >>= DebugLayer.hide

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
                                            Just blockIndex -> editBlock ref materials scene blockIndex block (Options options)

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


                    (OnKeyDown e) -> do
                        let go f = void do modifyRef ref \(State state) -> State (f state true)
                        liftEff do
                            case key e of
                                " " -> go _ { spaceKey = _ }
                                "w" -> go _ { wKey = _ }
                                "s" -> go _ { sKey = _ }
                                "a" -> go _ { aKey = _ }
                                "d" -> go _ { dKey = _ }
                                "r" -> go _ { rKey = _ }
                                "f" -> go _ { fKey = _ }
                                "q" -> go _ { qKey = _ }
                                "e" -> go _ { eKey = _ }
                                "t" -> go _ { tKey = _ }
                                "g" -> go _ { gKey = _ }
                                _ -> pure unit
                            preventDefault (keyboardEventToEvent e)
                            stopPropagation (keyboardEventToEvent e)

                    (OnKeyUp e) -> do
                        let go f = void do modifyRef ref \(State state) -> State (f state false)
                        liftEff do
                            case key e of
                                " " -> go _ { spaceKey = _ }
                                "w" -> go _ { wKey = _ }
                                "s" -> go _ { sKey = _ }
                                "a" -> go _ { aKey = _ }
                                "d" -> go _ { dKey = _ }
                                "r" -> go _ { rKey = _ }
                                "f" -> go _ { fKey = _ }
                                "q" -> go _ { qKey = _ }
                                "e" -> go _ { eKey = _ }
                                "t" -> go _ { tKey = _ }
                                "g" -> go _ { gKey = _ }
                                _ -> pure unit
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
                            stopAllSounds sounds
                        modifyAppState ref (\(State state) -> State state { sceneState = nextScene })
                        wait 1000
                        liftEff do
                            play sounds.cleaning
                        modifyAppState ref (\(State state) -> State state { nextScene = Nothing })
        pure next




modifyAppState :: forall eff. Ref State -> (State -> State) -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
modifyAppState ref f = do
    state <- liftEff $ readRef ref
    let state' = (f state)
    liftEff $ writeRef ref state'
    put state'



wait :: forall m eff. (MonadAff (timer :: TIMER | eff) m) => Int -> m Unit
wait msecs = liftAff (makeAff \reject resolve -> void (setTimeout msecs (resolve unit)))


offsetX :: MouseEvent -> Int
offsetX e = (unsafeCoerce e).offsetX

offsetY :: MouseEvent -> Int
offsetY e = (unsafeCoerce e).offsetY

deltaY :: MouseEvent -> Int
deltaY e = (unsafeCoerce e).deltaY

