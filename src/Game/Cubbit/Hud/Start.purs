module Game.Cubbit.Hud.Start (start, clearTerrain, modifyAppState) where

import Control.Alt (void)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef)
import Control.Monad.Rec.Class (Step(..), tailRecM2)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.Aff (wait)
import Game.Cubbit.BlockIndex (blockIndex)
import Game.Cubbit.Chunk (Chunk(..))
import Game.Cubbit.ChunkIndex (chunkIndex, runChunkIndex)
import Game.Cubbit.ChunkInstance (createChunkWithMesh, disposeChunk)
import Game.Cubbit.ChunkMap (toList)
import Game.Cubbit.ChunkMap (insert) as CHUNKMAP
import Game.Cubbit.Collesion (buildCollesionTerrain)
import Game.Cubbit.Firebase (listenOnceToTerrainAff, listenToTerrain)
import Game.Cubbit.Hud.Type (HudEffects, Query)
import Game.Cubbit.MeshBuilder (generateChunk, putBlocks)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Storage (listenAllChunks)
import Game.Cubbit.Terrain (Terrain(Terrain), createTerrain)
import Game.Cubbit.Types (GameMode(MultiplayerMode, SinglePlayerMode), Mode(Move), SceneState(PlayingSceneState), State(State))
import Graphics.Babylon.AbstractMesh (setIsVisible)
import Graphics.Babylon.Sound (play)
import Graphics.Babylon.Types (BABYLON)
import Graphics.Cannon (CANNON, removeBody)
import Graphics.Cannon.Type (World)
import Halogen (ComponentDSL, liftEff, put)
import Prelude (bind, negate, pure, ($), (-), (<$>))

start :: forall eff. Ref State -> State -> Resources -> GameMode -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
start ref (State currentState) res@{ options: Options options } gameMode = do

    -- fade out --
    liftEff $ play res.sounds.warpSound
    modifyAppState ref (\(State state) -> State state { nextScene = true })
    wait 1000

    -- initialize play screen --
    Terrain emptyTerrain <- liftEff do
        clearTerrain currentState.terrain currentState.world
        createTerrain 0

    reference <- case gameMode of
        SinglePlayerMode -> pure Nothing
        MultiplayerMode -> liftAff do

            -- fetch initial terrain state from firebase
            initialChunks <- listenOnceToTerrainAff res.firebase

            liftEff do

                -- insert chunkMesh object to the terrain
                for_ initialChunks \(Chunk chunk) -> do
                    let i = runChunkIndex chunk.index
                    CHUNKMAP.insert chunk.index (createChunkWithMesh (Chunk chunk) true) emptyTerrain.map

                -- start listening events
                r <- Just <$> listenToTerrain res.firebase \(Chunk chunk) -> do
                    -- CHUNKMAP.insert chunk.index (createChunkWithMesh (Chunk chunk)) emptyTerrain.map
                    -- TODO refresah nighbor chunks
                    putBlocks ref res (Chunk chunk)

                    pure unit

                pure r

    modifyAppState ref (\(State state) -> State state {
        terrain = Terrain emptyTerrain,
        cameraPosition = { x: 10.0, y: 20.0, z: negate 10.0 },
        cameraTarget = { x: 0.5, y: 11.0, z: 0.5 },
        sceneState = PlayingSceneState {
            gameMode: gameMode,
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
            jumpable: true,

            cursorPosition: blockIndex 0 0 0,
            centerPanelVisible: false,
            life: 10,
            maxLife: 12,

            ref: reference
        }
    })

    liftEff do

        -- load neighbor meshes
        let initialWorldSize = options.initialWorldSize
        forE (-initialWorldSize) initialWorldSize \x -> do
            forE (-initialWorldSize) initialWorldSize \z -> void do
                let index = chunkIndex x 0 z
                State state <- readRef ref
                generateChunk (State state) res.materials res.scene index res.options state.config

        -- initialize cannon world
        State { world } <- readRef ref
        terrain <- tailRecM2 (\ter -> case _ of
            0 -> pure $ Done ter
            i -> do
                ter' <- buildCollesionTerrain ter world (chunkIndex 0 0 0)
                pure $ Loop { a: ter', b: i - 1 }
        ) (Terrain emptyTerrain) 9
        modifyRef ref \(State state) -> State state { terrain = terrain }

        for_ res.playerMeshes \mesh -> void do
            setIsVisible true mesh

        case gameMode of
            SinglePlayerMode -> listenAllChunks $ putBlocks ref res
            MultiplayerMode -> pure unit

    wait 1000
    liftEff do
        play res.sounds.forestSound

        reportToSentry

    modifyAppState ref (\(State state) -> State state {
        nextScene = false,
        nextBGM = Just res.sounds.rye
    })






modifyAppState :: forall eff. Ref State -> (State -> State) -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
modifyAppState ref f = do
    state <- liftEff $ readRef ref
    let state' = (f state)
    liftEff $ writeRef ref state'
    put state'




clearTerrain :: forall eff. Terrain -> World -> Eff (babylon :: BABYLON, cannon :: CANNON | eff) Unit
clearTerrain (Terrain terrain) world = do
    -- dispose cannon bodies
    for_ terrain.bodies \chunkBodies -> for_ chunkBodies \body -> removeBody body world

    -- dispose meshes
    chunkList <- toList terrain.map
    for_ chunkList \chunk -> disposeChunk chunk


foreign import reportToSentry :: forall eff. Eff (dom :: DOM | eff) Unit
