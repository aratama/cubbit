module Game.Cubbit.Hud.Start (start, clearTerrain) where

import Control.Alt (void)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (Step(..), tailRecM2)
import Data.Array ((..))
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
import Game.Cubbit.Firebase (createTerrainRef, listenOnceToTerrainAff, listenToTerrain')
import Game.Cubbit.Hud.Type (HudEffects, Query)
import Game.Cubbit.MeshBuilder (generateChunk, putBlocks)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Storage (getAllChunks)
import Game.Cubbit.Terrain (Terrain(Terrain), createTerrain)
import Game.Cubbit.Types (GameMode(MultiplayerMode, SinglePlayerMode), Mode(Move), SceneState(PlayingSceneState), State(State))
import Graphics.Babylon.AbstractMesh (setIsVisible)
import Graphics.Babylon.Sound (play)
import Graphics.Babylon.Types (BABYLON)
import Graphics.Cannon (CANNON, removeBody)
import Graphics.Cannon.Type (World)
import Halogen (ComponentDSL, liftEff)
import Halogen.Query (get, modify)
import Prelude (bind, negate, pure, ($), (-))
import Raven (reportToSentry)

start :: forall eff. State -> Resources -> GameMode -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
start (State currentState) res@{ options: Options options } gameMode = do

    -- fade out --
    liftEff $ play res.sounds.warpSound
    modify \(State state) -> State state { nextScene = true }
    wait 1000

    -- initialize play screen --
    Terrain emptyTerrain <- liftEff do
        clearTerrain currentState.terrain res.world
        createTerrain 0

    reference <- case gameMode of
        SinglePlayerMode -> pure Nothing
        MultiplayerMode -> liftAff do

            -- fetch initial terrain state from firebase
            initialChunks <- listenOnceToTerrainAff res.firebase

            do

                -- insert chunkMesh object to the terrain
                for_ initialChunks \(Chunk chunk) -> do
                    let i = runChunkIndex chunk.index
                    liftEff $ CHUNKMAP.insert chunk.index (createChunkWithMesh (Chunk chunk) true) emptyTerrain.map

                -- start listening events
                r <- liftEff $ createTerrainRef res.firebase
                pure $ Just r

    modify \(State state) -> State state {
        terrain = Terrain emptyTerrain,
        cameraPosition = { x: 10.0, y: 20.0, z: negate 10.0 },
        cameraTarget = { x: 0.5, y: 11.0, z: 0.5 },
        sceneState = PlayingSceneState {
            res,
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
    }

    -- load neighbor meshes
    let initialWorldSize = options.initialWorldSize
    for_ ((-initialWorldSize) .. initialWorldSize) \x -> do
        for_ ((-initialWorldSize) .. initialWorldSize) \z -> void do
            let index = chunkIndex x 0 z
            State state <- get
            liftEff $ generateChunk (State state) res.materials res.scene index res.options state.config res


    -- initialize cannon world
    initialTerrain <- tailRecM2 (\ter -> case _ of
        0 -> pure $ Done ter
        i -> do
            ter' <- liftEff $ buildCollesionTerrain ter res.world (chunkIndex 0 0 0)
            pure $ Loop { a: ter', b: i - 1 }
    ) (Terrain emptyTerrain) 9
    modify \(State state) -> State state { terrain = initialTerrain }





    liftEff do
        for_ res.playerMeshes \mesh -> void do
            setIsVisible true mesh

    case gameMode of
        SinglePlayerMode -> do
            chunks <- liftAff $ makeAff \_ resolve -> getAllChunks resolve
            for_ chunks $ \chunk -> do
                State s <- get
                terrain <- liftEff $ putBlocks (State s) res chunk
                modify \(State state) -> State state { terrain = terrain }
        MultiplayerMode -> pure unit

    wait 1000
    liftEff do
        play res.sounds.forestSound
        reportToSentry

    modify \(State state) -> State state {
        nextScene = false,
        nextBGM = Just res.sounds.rye
    }

    -- -----------------------------------------------------------
    -- **HACK** start listening
    -- Following sequence is called repeadly!
    ---------------------------------------------------------------------
    case reference of
        Nothing -> pure unit
        Just firebaseRef -> do
            Chunk chunk <- liftAff $ makeAff \_ resolve -> listenToTerrain' firebaseRef resolve
            liftEff $ CHUNKMAP.insert chunk.index (createChunkWithMesh (Chunk chunk) true) emptyTerrain.map
            -- TODO refresah nighbor chunks

            State s <- get
            editedTerrain <- liftEff $ putBlocks (State s) res (Chunk chunk)

            -- TODO
            modify \(State state) -> State state { terrain = editedTerrain }

clearTerrain :: forall eff. Terrain -> World -> Eff (babylon :: BABYLON, cannon :: CANNON | eff) Unit
clearTerrain (Terrain terrain) world = do
    -- dispose cannon bodies
    for_ terrain.bodies \chunkBodies -> for_ chunkBodies \body -> removeBody body world

    -- dispose meshes
    chunkList <- toList terrain.map
    for_ chunkList \chunk -> disposeChunk chunk


