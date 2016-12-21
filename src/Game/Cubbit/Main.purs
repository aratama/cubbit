module Game.Cubbit.Main (main) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef, writeRef)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, toNullable)
import Data.Set (empty)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Collesion (buildCollesionBoxes)
import Game.Cubbit.Config (Config(Config), readConfig)
import Game.Cubbit.Constants (sliderMaxValue)
import Game.Cubbit.Event (focus)
import Game.Cubbit.Hud.Driver (initializeHud)
import Game.Cubbit.Hud.Eval (repaint)
import Game.Cubbit.MeshBuilder (generateChunk)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (loadResources, resourceCount)
import Game.Cubbit.Sounds (setBGMVolume, setMute, setSEVolume)
import Game.Cubbit.Terrain (Terrain(..), createTerrain, lookupChunk)
import Game.Cubbit.Types (Effects, ResourceProgress(..), SceneState(..), State(State))
import Game.Cubbit.Update (update, updateBabylon)
import Graphics.Babylon.Engine (getDeltaTime, resize, runRenderLoop)
import Graphics.Babylon.Scene (render)
import Graphics.Babylon.Sound (play)
import Graphics.Babylon.Types (BABYLON)
import Graphics.Babylon.Util (querySelectorCanvas)
import Graphics.Cannon (World)
import Graphics.Cannon (createWorld, createBox, createVec3, defaultBodyProps, createBody, addShape, addBody, step, setTag, getTag, getPosition, setPosition, runVec3, setGravity, setVelocity, getVelocity, createMaterial, setFixedRotation, updateMassProperties, createSphere) as CANNON
import Graphics.Cannon.Type (CANNON)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Util (runHalogenAff)
import Prelude (negate, void, (#), ($), (/), (<$>), (>>=), (+), (-), (<>))

main :: forall eff. Eff (Effects eff) Unit
main = (toMaybe <$> querySelectorCanvas "#renderCanvas") >>= case _ of
    Nothing -> error "canvas not found"
    Just canvasGL -> runHalogenAff do

        -- NOTE: have to do awaitBody before getting options
        bodyElement <- awaitBody

        -- config
        Config config <- liftEff $ readConfig

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
                cameraPosition: { x: 10.0, y: 20.0, z: negate 10.0 },
                cameraTarget: { x: 0.5, y: 11.0, z: 0.5 },
                mousePosition: { x: 0, y: 0 },
                debugLayer: false,
                minimap: false,
                totalFrames: 0,
                keys: empty
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






            -- load initial chunks
            do
                let initialWorldSize = options.initialWorldSize
                forE (-initialWorldSize) initialWorldSize \x -> do
                    forE (-initialWorldSize) initialWorldSize \z -> void do
                        let index = chunkIndex x 0 z
                        State state <- readRef ref
                        generateChunk (State state) materials scene index (Options options) (Config config)



            ----------------------------------------------------------------------------------
            ----------------------------------------------------------------------------------
            --------------------------------------------------------------------------------
            -- test collesion
            -- cannon
            world <- CANNON.createWorld
            gravity <- CANNON.createVec3 0.0 options.gravity 0.0
            CANNON.setGravity gravity world

            playerBox <- do
                size <- CANNON.createVec3 0.5 0.5 0.5
                pos <- CANNON.createVec3 2.5 17.0 2.5
                mat <- CANNON.createMaterial {
                    friction: 0.0000,
                    restitution: 0.0
                }

                body <- CANNON.createBody CANNON.defaultBodyProps {
                    mass = 1.0,
                    material = mat
                }

                upper <- CANNON.createSphere 0.4
                upperOffset <- CANNON.createVec3 0.0 (1.2) 0.0
                CANNON.addShape upper (Just upperOffset) Nothing body
                lower <- CANNON.createSphere 0.4
                lowerOffset <- CANNON.createVec3 0.0 (0.4) 0.0
                CANNON.addShape lower (Just lowerOffset) Nothing body

                CANNON.setPosition pos body
                CANNON.setTag (Just "player") body
                CANNON.setFixedRotation true body
                CANNON.updateMassProperties body
                CANNON.addBody body world
                pure body


            forE (-1) 2 \x -> do
                forE (-1) 2 \z -> void do
                    centerChunkMaybe <- lookupChunk (chunkIndex x 0 z) initialState.terrain
                    case centerChunkMaybe of
                        Nothing -> pure unit
                        Just chunk -> do
                            cannonBodies <- buildCollesionBoxes chunk world
                            pure unit

            ----------------------------------------------------------------
            ------------------------------------------------------------------------
            ---------------------------------------------------------------------------


            -- focus
            focus "content"

            -- start bgm
            setMute config.mute sounds
            setBGMVolume (toNumber config.bgmVolume / toNumber sliderMaxValue) sounds
            setSEVolume (toNumber config.seVolume / toNumber sliderMaxValue) sounds
            play sounds.cleaning



            -- resize
            win <- window
            addEventListener (EventType "resize") (eventListener $ \e -> do
                resize engine
            ) false (windowToEventTarget win)

            -- start game loop
            engine # runRenderLoop do
                deltaTime <- getDeltaTime engine
                State state <- readRef ref
                State state' <- update ref (State state) deltaTime scene sounds cursor (Options options) driver
                writeRef ref (State state')
                State state'' <- updateBabylon (State state') engine scene materials sounds shadowMap cursor targetCamera (Options options) skybox driver
                writeRef ref (State state'')
                render scene

                deltaTimeInMilliseconds <- getDeltaTime engine

                State state <- readRef ref
                case state.sceneState of
                    TitleSceneState t -> pure unit
                    PlayingSceneState p -> do

                        log ("py: " <> show p.position.y)
                        log ("vy: " <> show p.velocity.y)

                        -- apply state to cannon world
                        pos <- CANNON.createVec3 p.position.x p.position.y p.position.z
                        CANNON.setPosition pos playerBox
                        velocity <- CANNON.createVec3 p.velocity.x p.velocity.y p.velocity.z
                        CANNON.setVelocity velocity playerBox

                        -- step the world
                        CANNON.step (1.0 / 60.0) (1000.0 / deltaTimeInMilliseconds) 10 world

                        -- read stepped world state
                        pos <- CANNON.getPosition playerBox >>= CANNON.runVec3
                        vel <- CANNON.getVelocity playerBox >>= CANNON.runVec3
                        modifyRef ref \(State state) -> State state {
                            sceneState = PlayingSceneState p {
                                position = { x: pos.x, y: pos.y, z: pos.z },
                                velocity = vel
                            }
                        }




buildCollesionBodies :: forall a eff. World String -> Terrain -> Int -> Int -> Int -> Eff (cannon :: CANNON, babylon :: BABYLON | eff) Unit
buildCollesionBodies world terrain x y z = do
    centerChunkMaybe <- lookupChunk (chunkIndex x y z) terrain
    case centerChunkMaybe of
        Just chunk@{ bodies: Nothing } -> do
            cannonBodies <- buildCollesionBoxes chunk world
            pure unit

        _ -> pure unit

