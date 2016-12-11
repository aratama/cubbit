module Game.Cubbit.Hud.Eval (eval, setMute) where

import Control.Alt (void)
import Control.Alternative (when)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import DOM.Event.Event (preventDefault, stopPropagation)
import DOM.Event.KeyboardEvent (key, keyboardEventToEvent)
import DOM.Event.MouseEvent (MouseEvent, buttons)
import Data.BooleanAlgebra (not)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Ord (max, min)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockType (airBlock)
import Game.Cubbit.Config (Config(Config), writeConfig)
import Game.Cubbit.Control (pickBlock)
import Game.Cubbit.Hud.Type (Query(..), HudEffects, HudState, GameScene(..), PlayingSceneQuery(..))
import Game.Cubbit.Materials (Materials)
import Game.Cubbit.MeshBuilder (editBlock)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.PointerLock (exitPointerLock, requestPointerLock)
import Game.Cubbit.Sounds (Sounds)
import Game.Cubbit.Types (Mode(..), State(..), SceneState(..))
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.Scene (getDebugLayer)
import Graphics.Babylon.Sound (play, setVolume)
import Graphics.Babylon.Types (BABYLON, Mesh, Scene)
import Halogen (ComponentDSL, liftAff, liftEff, modify)
import Halogen.Query (get)
import Math (pi)
import Prelude (type (~>), bind, negate, pure, ($), (*), (+), (-), (/=), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)


eval :: forall eff. Scene -> Mesh -> Materials -> Options -> Ref State -> Sounds -> (Query ~> ComponentDSL HudState Query Void (Aff (HudEffects eff)))
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
        warpToNextScene sounds PlayingScene
        pure next


    (ToggleMute next) -> do
        modify \state -> state { mute = not state.mute }
        state <- get
        liftEff do
            setMute state.mute sounds
            writeConfig (Config { mute: state.mute })
        pure next

    (PlayingSceneQuery playingSceneQuery next) -> do

        -- todo
        State gameState <- liftEff $ readRef ref
        case gameState.sceneState of
            TitleSceneState -> pure unit
            PlayingSceneState playingSceneState -> do
                pure unit



        case playingSceneQuery of

            (SetCursorPosition position) -> do
                state <- get
                when (position /= state.cursorPosition) do
                    modify (_ { cursorPosition = position })

            (SetMode mode) -> do
                liftEff do
                    State s <- readRef ref
                    when (s.mode /= mode) do
                        writeRef ref (State s { mode = mode })
                        play sounds.switchSound
                modify _ { mode = mode }

            (SetPosition position) -> do








                liftEff (modifyRef ref (\(State s) -> State s { position = position }))

            TogglePointerLock -> do
                liftEff do
                    modifyRef ref (\(State state) -> State state { firstPersonView = not state.firstPersonView })
                    State state <- readRef ref
                    if state.firstPersonView
                        then requestPointerLock (\e -> do
                            modifyRef ref (\(State state) -> State state {
                                playerRotation = state.playerRotation + e.movementX * options.pointerHorizontalSensitivity,
                                playerPitch = max (-pi * 0.45) (min (pi * 0.45) state.playerPitch - e.movementY * options.pointerVerticalSensitivity)
                            })
                            pure unit
                        ) (modifyRef ref (\(State state) -> State state {
                            firstPersonView = false
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
                                        cameraYaw = if isRightButton then state.cameraYaw + toNumber dx * options.cameraHorizontalSensitivity else state.cameraYaw,
                                        cameraPitch = if isRightButton then max (-pi * 0.45) $ min (pi * 0.45) $ state.cameraPitch + toNumber dy * options.cameraVertialSensitivity else state.cameraPitch
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
                                picked <- pickBlock scene cursor (State state) state.mousePosition.x state.mousePosition.y
                                case picked of
                                    Nothing -> pure unit
                                    Just blockIndex -> editBlock ref materials scene blockIndex block (Options options)

                        case state.mode of
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
                        cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (state.cameraRange + (toNumber (deltaY e) * options.cameraZoomSpeed)))
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
                modify \state -> state { centerPanelVisible = visible }



            Home -> do
                warpToNextScene sounds TitleScene

        pure next

warpToNextScene :: forall eff. Sounds -> GameScene -> ComponentDSL HudState Query Void (Aff (HudEffects eff)) Unit
warpToNextScene sounds nextScene = do
    liftEff $ play sounds.warpSound
    modify (_ { nextScene = Just nextScene })
    wait 1000
    modify (_ { gameScene = nextScene })
    wait 100
    modify (_ { nextScene = Nothing })

wait :: forall m eff. (MonadAff (timer :: TIMER | eff) m) => Int -> m Unit
wait msecs = liftAff (makeAff \reject resolve -> void (setTimeout msecs (resolve unit)))

setMute :: forall eff. Boolean -> Sounds -> Eff (babylon :: BABYLON | eff) Unit
setMute mute sounds = do
    let go = setVolume (if mute then 0.0 else 1.0)
    go sounds.forestSound
    go sounds.switchSound
    go sounds.pickSound
    go sounds.putSound

offsetX :: MouseEvent -> Int
offsetX e = (unsafeCoerce e).offsetX

offsetY :: MouseEvent -> Int
offsetY e = (unsafeCoerce e).offsetY

deltaY :: MouseEvent -> Int
deltaY e = (unsafeCoerce e).deltaY

