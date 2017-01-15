module Game.Cubbit.Hud.Eval (eval) where

import Control.Monad.Aff (Aff)
import Control.Monad.RWS (modify)
import DOM.Event.Event (preventDefault, stopPropagation)
import Data.BooleanAlgebra (not)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Unit (unit)
import Data.Void (Void)
import Game.Cubbit.Aff (wait)
import Game.Cubbit.Config (Config(Config), writeConfig)
import Game.Cubbit.Hud.Terrain (initializeTerrain)
import Game.Cubbit.Hud.EvalPlayingSceneQuery (evalPlayingSceneQuery)
import Game.Cubbit.Hud.Gameloop (gameloop)
import Game.Cubbit.Hud.ModeSelect (modeSelect)
import Game.Cubbit.Hud.Start (start)
import Game.Cubbit.Hud.Type (HudEffects, Query(Query, LoadResources), QueryA(PlayingSceneQuery, ToggleMute, Start, ModeSelect, Home, SetChunkArea, SetShadowArea, ToggleWaterMaterial, ToggleVertexColor, ToggleShadow, SetSEVolume, SetBGMVolume, CloseConfig, ShowConfig, StopPropagation, Nop, PreventDefault, SetLanguage, SetActiveGameMode, Repaint, Gameloop), getRes)
import Game.Cubbit.Resources (loadResourcesH)
import Game.Cubbit.Types (SceneState(TitleSceneState, ModeSelectionSceneState, LoadingSceneState), State(State))
import Graphics.Babylon.AbstractMesh (setReceiveShadows, setUseVertexColors)
import Graphics.Babylon.Scene (getMeshes)
import Graphics.Babylon.Sound (play, stop)
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (get)
import Prelude (type (~>), bind, pure, ($), (<$), (+), (<$>))

eval :: forall eff. (Query ~> ComponentDSL State Query Void (Aff (HudEffects eff)))
eval query = case query of

    LoadResources f -> f <$> do

        res <- loadResourcesH do
            modify \(State state) -> State state {
                sceneState = case state.sceneState of
                    LoadingSceneState count -> LoadingSceneState $ count + 1
                    x -> x
            }

        modify \(State state) -> State state {
            nextBGM = Just res.sounds.cleaning,
            sceneState = TitleSceneState {
                res: res,
                position: 0.0
            }
        }

        -- clear terrain mesh and terrain bodies
        initializeTerrain res

        pure res

    Query q next -> next <$ case q of

        Gameloop res -> gameloop res

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
            modeSelect res

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
