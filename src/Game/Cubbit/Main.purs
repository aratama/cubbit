module Game.Cubbit.Main (main) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef, writeRef)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM2)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, toNullable)
import Data.Set (empty)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Collesion (buildCollesionBoxes, updatePhysics, createPlayerCollesion, buildCollesionTerrain)
import Game.Cubbit.Config (Config(Config), readConfig)
import Game.Cubbit.Event (focus)
import Game.Cubbit.Hud.Driver (initializeHud)
import Game.Cubbit.Hud.Eval (repaint, initializeTerrain)
import Game.Cubbit.MeshBuilder (generateChunk, putBlocks)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (loadResources, resourceCount)
import Game.Cubbit.Storage (listenAllChunks, listenAllChunksFromForebase)
import Game.Cubbit.Terrain (Terrain(Terrain), createTerrain, globalPositionToChunkIndex)
import Game.Cubbit.Types (Effects, ResourceProgress(..), SceneState(..), State(State))
import Game.Cubbit.Update (update, updateBabylon, updateSound)
import Graphics.Babylon.Engine (getDeltaTime, resize, runRenderLoop)
import Graphics.Babylon.Scene (render)
import Graphics.Babylon.Util (querySelectorCanvas)
import Graphics.Cannon (addBody, createVec3, createWorld, setGravity)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Util (runHalogenAff)
import Prelude (negate, void, (#), ($), (+), (-), (<$>), (<>), (>>=))


main :: forall eff. Eff (Effects eff) Unit
main = (toMaybe <$> querySelectorCanvas "#renderCanvas") >>= case _ of
    Nothing -> error "canvas not found"
    Just canvasGL -> runHalogenAff do

        -- NOTE: have to do awaitBody before getting options
        bodyElement <- awaitBody

        -- config
        Config config <- liftEff $ readConfig

        world <- liftEff $ createWorld

        -- initialize game state
        let terrainSeed = 0
        initialTerrain <- liftEff $ createTerrain terrainSeed
        let initialState =  {
                config: Config config,
                res: Loading 0,
                configVisible: false,
                sceneState: TitleSceneState {
                    position: 0.0
                },
                nextScene: Nothing,
                skyboxRotation: 0.0,
                terrain: initialTerrain,
                updateIndex: toNullable Nothing,
                world,
                cameraPosition: { x: 10.0, y: 20.0, z: negate 10.0 },
                cameraTarget: { x: 0.5, y: 11.0, z: 0.5 },
                mousePosition: { x: 0, y: 0 },
                debugLayer: false,
                minimap: false,
                totalFrames: 0,
                keys: empty,
                bgm: Nothing,
                nextBGM: Nothing,
                volume: 1.0
            }

        ref <- liftEff $ newRef $ State initialState

        -- initialize ui
        driver <- initializeHud (State initialState) ref bodyElement

        -- load resources
        incref <- liftEff $ newRef 0
        let inc = do
                liftEff $ modifyRef incref (_ + 1)
                count <- liftEff $ readRef incref
                liftEff $ log $ show count <> " / " <> show resourceCount
                State state <- liftEff $ readRef ref
                repaint driver $ State state {
                    res = Loading count
                }
        res <- loadResources canvasGL inc
        repaint driver $ State initialState { res = Complete res }
        let engine = res.engine
        let scene = res.scene
        let playerMeshes = res.playerMeshes
        let sounds = res.sounds
        let cursor = res.cursor
        let skybox = res.skybox
        let materials = res.materials
        let targetCamera = res.targetCamera
        let shadowMap = res.shadowMap
        Options options <- pure res.options




        liftEff do
            modifyRef ref \(State state) -> State state {
                nextBGM = Just sounds.cleaning
            }

            -- cannon --

            gravity <- createVec3 0.0 options.gravity 0.0
            setGravity gravity world

            playerBox <- createPlayerCollesion
            addBody playerBox world

            initializeTerrain ref

            -- focus
            focus "content"

            -- resize
            win <- window
            addEventListener (EventType "resize") (eventListener $ \e -> do
                resize engine
            ) false (windowToEventTarget win)

            -- start game loop
            engine # runRenderLoop do

                let updateCanvas = do

                        deltaTime <- getDeltaTime engine

                        readRef ref >>=
                            update deltaTime scene sounds cursor (Options options) driver >>=
                                updateBabylon deltaTime scene materials sounds shadowMap cursor targetCamera (Options options) skybox driver >>=
                                    updateSound deltaTime scene materials sounds shadowMap cursor targetCamera (Options options) skybox driver >>=
                                        updatePhysics deltaTime playerBox world >>=
                                            writeRef ref

                        do
                            State s <- readRef ref
                            case s.sceneState of
                                PlayingSceneState p -> do
                                    let index = globalPositionToChunkIndex p.position.x p.position.y p.position.z
                                    Terrain t <- buildCollesionTerrain s.terrain world index
                                    -- log $ "boxes: " <> (show $ sum $ (\(Tuple k v) -> length v) <$> toList t.bodies)
                                    modifyRef ref \(State state) -> State state { terrain = Terrain t }
                                _ -> pure unit

                        render scene

                State st <- readRef ref
                case st.sceneState of
                    TitleSceneState _ -> updateCanvas
                    ModeSelectionSceneState _ -> do
                        deltaTime <- getDeltaTime engine
                        readRef ref >>=
                            updateSound deltaTime scene materials sounds shadowMap cursor targetCamera (Options options) skybox driver >>=
                                writeRef ref
                    PlayingSceneState _ -> updateCanvas






