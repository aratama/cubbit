module Game.Cubbit.Hud.Eval (eval) where


import Control.Monad.Aff (Aff)
import Control.Monad.RWS (modify)
import DOM.Event.Event (preventDefault, stopPropagation)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.WheelEvent (WheelEvent)
import Data.BooleanAlgebra (not)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.Aff (wait)
import Game.Cubbit.Config (Config(Config), writeConfig)
import Game.Cubbit.Hud.Gameloop (gameloop, initializeTerrain)
import Game.Cubbit.Hud.Start (start)
import Game.Cubbit.Hud.Type (HudDriver, HudEffects, Query(Query), QueryA(Repaint, PlayingSceneQuery, ToggleMute, Start, ModeSelect, Home, SetChunkArea, SetShadowArea, ToggleWaterMaterial, ToggleVertexColor, ToggleShadow, SetSEVolume, SetBGMVolume, CloseConfig, ShowConfig, StopPropagation, Nop, PreventDefault, SetLanguage, SetActiveGameMode, Gameloop), getRes)
import Game.Cubbit.Hud.EvalPlayingSceneQuery (evalPlayingSceneQuery)
import Game.Cubbit.Types (GameMode(SinglePlayerMode), SceneState(ModeSelectionSceneState, TitleSceneState), State(State))
import Graphics.Babylon.AbstractMesh (setReceiveShadows, setUseVertexColors)
import Graphics.Babylon.Scene (getMeshes)
import Graphics.Babylon.Sound (play, stop)
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (action, get)
import Prelude (type (~>), bind, pure, ($), (<$))
import Unsafe.Coerce (unsafeCoerce)

eval :: forall eff. (Query ~> ComponentDSL State Query Void (Aff (HudEffects eff)))
eval query = case query of

    Query q next -> next <$ case q of

        Gameloop -> gameloop

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
            initializeTerrain res
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
            start (State state) res gameMode

        (ToggleMute) -> do
            modify \(State state@{ config: Config config }) -> State state {
                config = Config config {
                    mute = not config.mute
                }
            }

            State state@{ config: Config config } <- get
            liftEff do
                writeConfig state.config

        (PlayingSceneQuery playingSceneQuery) -> evalPlayingSceneQuery playingSceneQuery
