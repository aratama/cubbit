module Game.Cubbit.Main (main) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception (error) as EXP
import Control.Monad.Eff.Ref (newRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toMaybe, toNullable)
import Data.Show (show)
import Data.String (Pattern(..), contains)
import Data.Unit (Unit)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Config (Config(Config), readConfig)
import Game.Cubbit.Constants (skyBoxRenderingGruop)
import Game.Cubbit.Event (focus)
import Game.Cubbit.Hud.Driver (initializeHud, queryToHud)
import Game.Cubbit.Hud.Type (Query(..))
import Game.Cubbit.Materials (initializeMaterials)
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Option (Options(Options), readOptions)
import Game.Cubbit.Sounds (loadSounds)
import Game.Cubbit.Terrain (emptyTerrain)
import Game.Cubbit.Types (Effects, Mode(Move), SceneState(..), State(State))
import Game.Cubbit.Update (update)
import Graphics.Babylon.AbstractMesh (setIsPickable, setIsVisible, getSkeleton, setMaterial, setPosition, setReceiveShadows, setRenderingGroupId)
import Graphics.Babylon.Aff.SceneLoader (loadMesh)
import Graphics.Babylon.Aff.Texture (loadTexture)
import Graphics.Babylon.Camera (setFOV, setMaxZ, setMinZ)
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (createBox, meshToAbstractMesh, setInfiniteDistance)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.Scene (createScene, fOGMODE_EXP, render, setActiveCamera, setActiveCameras, setCollisionsEnabled, setFogColor, setFogDensity, setFogMode)
import Graphics.Babylon.ShadowGenerator (createShadowGenerator, getShadowMap, setBias, setUsePoissonSampling)
import Graphics.Babylon.Sound (play)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.TargetCamera (createTargetCamera, setTarget, targetCameraToCamera)
import Graphics.Babylon.Texture (sKYBOX_MODE, setCoordinatesMode, defaultCreateTextureOptions)
import Graphics.Babylon.Types (VertexDataProps)
import Graphics.Babylon.Util (querySelectorCanvas)
import Graphics.Babylon.Vector3 (createVector3)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Util (runHalogenAff)
import Halogen.Query (action)
import Network.HTTP.Affjax (get)
import Prelude (negate, void, (#), ($), (/), (<$>), (>>=))
import Unsafe.Coerce (unsafeCoerce)


main :: forall eff. Eff (Effects eff) Unit
main = (toMaybe <$> querySelectorCanvas "#renderCanvas") >>= case _ of
    Nothing -> error "canvasGL not found"
    Just canvasGL -> runHalogenAff do

        -- NOTE: have to do awaitBody before getting options
        body <- awaitBody

        -- load options
        response <- get "options.json"
        Options options <- case runExcept (readOptions response.response) of
            Left err -> throwError (EXP.error (show err))
            Right opt -> pure opt

        -- liftEff $ log (toJSONGeneric (defaultOptions { unwrapNewtypes  = true }) (Options options))

        engine <- liftEff $ createEngine canvasGL true

        scene <- liftEff do
            sce <- createScene engine
            setFogMode fOGMODE_EXP sce
            setFogDensity options.fogDensity sce
            fogColor <- createColor3 (155.0 / 255.0) (181.0 / 255.0) (230.0 / 255.0)
            setFogColor fogColor sce
            setCollisionsEnabled true sce
            pure sce

        loadImage "./title.png"
        loadImage "./screenshade.png"

        texture <- loadTexture "./texture.png" scene defaultCreateTextureOptions
        alphaTexture <- loadTexture "./alpha.png" scene defaultCreateTextureOptions
        loadTexture "./alice/texture.png" scene defaultCreateTextureOptions             -- make sure the texture loaded
        playerMeshes <- loadMesh "" "./alice/" "alice.babylon" scene pure
        sounds <- loadSounds scene

        cursor <- liftEff do
            cursorbox <- createBox "cursor" 1.0 scene
            setRenderingGroupId 1 (meshToAbstractMesh cursorbox)
            setIsPickable false (meshToAbstractMesh cursorbox)
            setIsVisible false (meshToAbstractMesh cursorbox)

            mat <- createStandardMaterial "cursormat" scene
            setWireframe true (standardMaterialToMaterial mat)
            setZOffset (negate 0.01) (standardMaterialToMaterial mat)
            setMaterial (standardMaterialToMaterial mat) (meshToAbstractMesh cursorbox)
            pure cursorbox

        -- skybox
        skybox <- liftEff do
            skyBoxCubeTex <- createCubeTexture "skybox/skybox" scene
            setCoordinatesMode sKYBOX_MODE (cubeTextureToTexture skyBoxCubeTex)

            skyboxMaterial <- createStandardMaterial "skyBox/skybox" scene
            setFogEnabled false (standardMaterialToMaterial skyboxMaterial)
            setBackFaceCulling false skyboxMaterial
            setDisableLighting true skyboxMaterial
            skyDiffuse <- createColor3 0.0 0.0 0.0
            setDiffuseColor skyDiffuse skyboxMaterial
            skySpec <- createColor3 0.0 0.0 0.0
            setSpecularColor skySpec skyboxMaterial
            setReflectionTexture (cubeTextureToTexture skyBoxCubeTex) skyboxMaterial

            skyboxMesh <- createBox "skybox" 1000.0 scene
            setRenderingGroupId skyBoxRenderingGruop (meshToAbstractMesh skyboxMesh)
            setMaterial (standardMaterialToMaterial skyboxMaterial) (meshToAbstractMesh skyboxMesh)
            setInfiniteDistance true skyboxMesh
            pure skyboxMesh

        -- prepare materials
        materials <- liftEff $ initializeMaterials scene skybox texture alphaTexture (Options options)


        res <- get "./alice/alice.babylon"
        let vertexDataArray = flipFaces res.response

        Config config <- liftEff $ readConfig

        -- initialize game state
        initialTerrain <- liftEff $ emptyTerrain 0
        let initialState =  State {
                config: Config config,

                playerMeshes: playerMeshes,

                sceneState: TitleSceneState { position: 0.0 },
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

        ref <- liftEff $ newRef initialState

        -- initialize hud
        driver <- initializeHud initialState ref (Options options) body scene cursor materials sounds

        liftEff do

            -- initialize scene
            targetCamera <- do
                position <- createVector3 10.0 20.0 (negate 10.0)
                camera <- createTargetCamera "target-camera" position scene
                cameraTarget <- createVector3 0.0 8.0 0.0
                setTarget cameraTarget camera
                setMaxZ options.cameraMaxZ (targetCameraToCamera camera)
                setMinZ options.cameraMinZ (targetCameraToCamera camera)
                setFOV options.cameraFOV (targetCameraToCamera camera)
                pure camera


            setActiveCameras [targetCameraToCamera targetCamera] scene
            setActiveCamera (targetCameraToCamera targetCamera) scene

            do
                hemiPosition <- createVector3 0.0 1.0 0.0
                hemiLight <- createHemisphericLight "Hemi0" hemiPosition scene
                diffuse <- createColor3 0.6 0.6 0.6
                setDiffuse diffuse (hemisphericLightToLight hemiLight)

            shadowMap <- do
                -- create a basic light, aiming 0,1,0 - meaning, to the sky
                lightDirection <- createVector3 0.3 (negate 1.0) 0.5
                light <- createDirectionalLight "light1" lightDirection scene
                dirColor <- createColor3 0.8 0.8 0.8
                setDiffuse dirColor (directionalLightToLight light)

                -- shadow
                shadowGenerator <- createShadowGenerator options.shadowMapSize light  -- over 8192 pixel-size texture causes performance regressions
                setBias 0.000005 shadowGenerator
                setUsePoissonSampling true shadowGenerator
                getShadowMap shadowGenerator

            -- initialize player charactor mesh
            for_ playerMeshes \mesh -> void do
                p <- createVector3 0.5 13.0 0.5
                setPosition p mesh
                setRenderingGroupId 1 mesh
                setReceiveShadows true mesh
                skeleton <- getSkeleton mesh
                let name = getName (unsafeCoerce mesh)
                setMaterial (if contains (Pattern "-outline") name then materials.outlineMaterial else materials.cellShadingMaterial) mesh

                setIsVisible false mesh

            -- load initial chunks
            do
                let initialWorldSize = options.initialWorldSize
                forE (-initialWorldSize) initialWorldSize \x -> do
                    forE (-initialWorldSize) initialWorldSize \z -> void do
                        let index = chunkIndex x 0 z
                        createChunkMesh ref materials scene index (Options options)

            -- start game loop
            engine # runRenderLoop do

                -- driver.query (action RequstAnimationFrame)

                update ref engine scene materials sounds shadowMap cursor targetCamera (Options options) skybox driver
                render scene

            -- focus
            focus "content"


            play sounds.cleaning

            hideLoading

foreign import hideLoading :: forall eff. Eff (dom :: DOM | eff) Unit

foreign import flipFaces :: Foreign -> Array VertexDataProps

loadImage :: forall eff. String -> Aff (dom :: DOM | eff) HTMLElement
loadImage src = makeAff (loadImageEff src)


foreign import loadImageEff :: forall eff. String -> (Error -> Eff (dom :: DOM | eff) Unit) -> (HTMLElement -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
