module Game.Cubbit.Hud.Gameloop (gameloop) where

import Control.Bind (when)
import Control.Monad.Aff (Aff)
import Control.MonadPlus (guard)
import Data.Array (findIndex, length, (!!))
import Data.EuclideanRing (mod)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Data.Void (Void)
import Game.Cubbit.Collesion (updatePhysics)
import Game.Cubbit.Hud.ModeSelect (modeSelect)
import Game.Cubbit.Hud.Type (HudEffects, Query)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Types (GameMode(MultiplayerMode, SinglePlayerMode), SceneState(ModeSelectionSceneState, TitleSceneState), State(State))
import Game.Cubbit.Update (updateBabylon, updateH, updateSound)
import Gamepad (getGamepads, onButtonPress)
import Graphics.Babylon.Engine (getDeltaTime)
import Graphics.Babylon.Scene (render)
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (get)
import Prelude (bind, pure, ($), (+), (==), (>>=))


gameloop :: forall eff. Resources -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
gameloop res@{ options: Options options } = do

    let updateAll st = do
            liftEff do
                deltaTime <- getDeltaTime res.engine
                State st'' <- pure (State st) >>=
                    updateH deltaTime res >>=
                        updateBabylon deltaTime res >>=
                            updateSound res.sounds >>=
                                updatePhysics deltaTime res.playerBox res.world

                render res.scene

                pure $ State st''

    gamepads <- liftEff $ getGamepads
    State st <- get
    State st''' <- case st.sceneState of

        TitleSceneState titleSceneState -> do
            when (onButtonPress 0 0 st.gamepads gamepads) do
                modeSelect titleSceneState.res
            updateAll st

        ModeSelectionSceneState modeSelectionSceneState -> do
            State st' <- liftEff $ updateSound res.sounds (State st)
            let modeMaybe = do
                    guard $ onButtonPress 0 0 st.gamepads gamepads
                    let xs = [SinglePlayerMode, MultiplayerMode]
                    i <- findIndex (_ == modeSelectionSceneState.mode) xs
                    xs !! (mod (i + 1) (length xs))
            pure case modeMaybe of
                Nothing -> State st' {
                    gamepads = gamepads
                }
                Just mode -> State st' {
                    sceneState = ModeSelectionSceneState modeSelectionSceneState {
                        mode =  mode
                    },
                    gamepads = gamepads
                }

        _ -> updateAll st



    put $ State st''' {
        gamepads = gamepads
    }
