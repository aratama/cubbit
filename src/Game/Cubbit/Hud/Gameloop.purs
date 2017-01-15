module Game.Cubbit.Hud.Gameloop (gameloop) where

import Control.Bind (when)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.RWS (modify)
import Control.MonadPlus (guard)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (resize) as DOM
import DOM.HTML.HTMLElement (focus)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..))
import Data.Array (findIndex, length, (!!))
import Data.EuclideanRing (mod)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Traversable (traverse_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.Collesion (createPlayerCollesion, updatePhysics)
import Game.Cubbit.Hud.ModeSelect (modeSelect)
import Game.Cubbit.Hud.Terrain (initializeTerrain)
import Game.Cubbit.Hud.Type (HudEffects, Query)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (loadResourcesH)
import Game.Cubbit.Types (GameMode(MultiplayerMode, SinglePlayerMode), SceneState(ModeSelectionSceneState, TitleSceneState, LoadingSceneState), State(State))
import Game.Cubbit.Update (updateBabylon, updateH, updateSound)
import Gamepad (getGamepads, onButtonPress)
import Graphics.Babylon.Engine (getDeltaTime, resize, runRenderLoop)
import Graphics.Babylon.Scene (render)
import Graphics.Babylon.Util (querySelectorCanvasAff)
import Graphics.Cannon (addBody, setGravity)
import Graphics.Cannon.Vec3 (createVec3)
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (get, lift)
import Prelude (bind, pure, ($), (+), (<#>), (<$>), (<<<), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

gameloop :: forall eff. ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
gameloop = do

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

    let updateAll st = do
            liftEff do
                deltaTime <- getDeltaTime res.engine
                State st'' <- pure (State st) >>=
                    updateH deltaTime res >>=
                        updateBabylon deltaTime res >>=
                            updateSound res.sounds >>=
                                updatePhysics deltaTime playerBox state.world

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