module Game.Cubbit.Main (main) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, toNullable)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Config (Config(Config), readConfig)
import Game.Cubbit.Constants (sliderMaxValue)
import Game.Cubbit.Event (focus)
import Game.Cubbit.Hud.Driver (initializeHud)
import Game.Cubbit.Hud.Eval (repaint)
import Game.Cubbit.Hud.Type (Query(..))
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (loadResources, resourceCount)
import Game.Cubbit.Sounds (setBGMVolume, setMute, setSEVolume)
import Game.Cubbit.Terrain (emptyTerrain)
import Game.Cubbit.Types (Effects, ResourceProgress(..), SceneState(TitleSceneState), State(State))
import Game.Cubbit.Update (update)
import Graphics.Babylon.Engine (runRenderLoop)
import Graphics.Babylon.Scene (render)
import Graphics.Babylon.Sound (play)
import Graphics.Babylon.Util (querySelectorCanvas)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Util (runHalogenAff)
import Halogen.Query (action)
import Prelude (negate, void, (#), ($), (/), (<$>), (>>=), (+), (<>))

main :: forall eff. Eff (Effects eff) Unit
main = (toMaybe <$> querySelectorCanvas "#renderCanvas") >>= case _ of
    Nothing -> error "canvas not found"
    Just canvasGL -> runHalogenAff do

        -- NOTE: have to do awaitBody before getting options
        body <- awaitBody

        -- config
        Config config <- liftEff $ readConfig

        -- initialize game state
        initialTerrain <- liftEff $ emptyTerrain 0
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
                spaceKey: false,
                wKey: false,
                sKey: false,
                aKey: false,
                dKey: false,
                qKey: false,
                eKey: false,
                rKey: false,
                fKey: false,
                tKey: false,
                gKey: false
            }

        ref <- liftEff $ newRef $ State initialState

        -- initialize hud
        driver <- initializeHud (State initialState) ref body


        -- resources
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

        repaint driver $ State initialState { res = Complete res }

        liftEff do

            -- load initial chunks
            do
                let initialWorldSize = options.initialWorldSize
                forE (-initialWorldSize) initialWorldSize \x -> do
                    forE (-initialWorldSize) initialWorldSize \z -> void do
                        let index = chunkIndex x 0 z
                        createChunkMesh ref materials scene index (Options options) (Config config)

            -- start game loop
            engine # runRenderLoop do
                update ref engine scene materials sounds shadowMap cursor targetCamera (Options options) skybox driver
                render scene

            -- focus

            focus "content"

            -- start bgm
            setMute config.mute sounds
            setBGMVolume (toNumber config.bgmVolume / toNumber sliderMaxValue) sounds
            setSEVolume (toNumber config.seVolume / toNumber sliderMaxValue) sounds
            play sounds.cleaning
