module Game.Cubbit.Hud.Gameloop (gameloop, initializeTerrain) where

import Control.Bind (join)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.RWS (modify)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM2)
import Control.MonadPlus (guard)
import DOM.Event.EventTarget (addEventListener, eventListener)
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
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Traversable (for_, traverse_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Collesion (buildCollesionTerrain, createPlayerCollesion, updatePhysics)
import Game.Cubbit.Hud.Start (clearTerrain)
import Game.Cubbit.Hud.Type (HudEffects, Query)
import Game.Cubbit.MeshBuilder (generateChunk)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (Resources, loadResourcesH)
import Game.Cubbit.Terrain (Terrain(Terrain), createTerrain)
import Game.Cubbit.Types (GameMode(MultiplayerMode, SinglePlayerMode), SceneState(ModeSelectionSceneState, TitleSceneState, LoadingSceneState), State(State))
import Game.Cubbit.Update (updateBabylon, updateH, updateSound)
import Gamepad (Gamepad(..), GamepadButton(..), getGamepads)
import Graphics.Babylon.Engine (getDeltaTime, resize, runRenderLoop)
import Graphics.Babylon.Scene (render)
import Graphics.Babylon.Util (querySelectorCanvasAff)
import Graphics.Cannon (addBody, setGravity)
import Graphics.Cannon.Vec3 (createVec3)
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (get, lift)
import Prelude (bind, negate, pure, ($), (&&), (+), (-), (<#>), (<$>), (<<<), (==), (>>=))
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
    State st <- get
    case st.sceneState of
        ModeSelectionSceneState modeSelectionSceneState -> do
            gamepads <- liftEff $ getGamepads

            State st' <- liftEff $ updateSound res.sounds (State st)

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
                Nothing -> put (State st' { gamepads = gamepads })
                Just mode -> put $ State st' {
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
