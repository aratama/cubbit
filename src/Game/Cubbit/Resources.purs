module Game.Cubbit.Resources (Resources, loadResources, resourceCount) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except (runExcept, throwError)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Show (show)
import Data.String (Pattern(..), contains)
import Data.Unit (Unit)
import Game.Cubbit.Aff (loadImage)
import Game.Cubbit.Constants (skyBoxRenderingGruop)
import Game.Cubbit.Materials (Materials, initializeMaterials)
import Game.Cubbit.Option (Options(Options), readOptions)
import Game.Cubbit.Sounds (Sounds, loadSounds)
import Graphics.Babylon.AbstractMesh (setIsPickable, setIsVisible, getSkeleton, setMaterial, setPosition, setReceiveShadows, setRenderingGroupId)
import Graphics.Babylon.Aff.SceneLoader (loadMesh)
import Graphics.Babylon.Aff.Texture (loadTexture)
import Graphics.Babylon.Camera (setFOV, setMaxZ, setMinZ)
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (createBox, meshToAbstractMesh, setInfiniteDistance)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.Scene (createScene, fOGMODE_EXP, setActiveCamera, setActiveCameras, setCollisionsEnabled, setFogColor, setFogDensity, setFogMode)
import Graphics.Babylon.ShadowGenerator (createShadowGenerator, getShadowMap, setBias, setUsePoissonSampling)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.TargetCamera (createTargetCamera, setTarget, targetCameraToCamera)
import Graphics.Babylon.Texture (sKYBOX_MODE, setCoordinatesMode, defaultCreateTextureOptions)
import Graphics.Babylon.Types (AbstractMesh, BABYLON, Canvas, Engine, Mesh, Scene, ShadowMap, TargetCamera)
import Graphics.Babylon.Vector3 (createVector3)
import Network.HTTP.Affjax (AJAX, get)
import Prelude (negate, void, ($), (/))
import Unsafe.Coerce (unsafeCoerce)
import Web.Firebase (FIREBASE, Firebase, initializeApp)

type Resources = {
    options :: Options,
    engine :: Engine,
    scene :: Scene,
    skybox :: Mesh,
    cursor :: Mesh,
    materials :: Materials,
    shadowMap :: ShadowMap,
    targetCamera :: TargetCamera,
    playerMeshes :: Array AbstractMesh,
    sounds :: Sounds,
    firebase :: Firebase
}

-- Note: Keep the number up-to-date
resourceCount :: Int
resourceCount = 21

loadResources :: forall eff. Canvas
        -> Aff (ajax :: AJAX, console :: CONSOLE, ref :: REF, dom :: DOM, babylon :: BABYLON, firebase :: FIREBASE | eff) Unit
        -> Aff (ajax :: AJAX, console :: CONSOLE, ref :: REF, dom :: DOM, babylon :: BABYLON, firebase :: FIREBASE | eff) Resources
loadResources canvasGL inc = do

    let loadImage' url = do
            img <- loadImage url
            inc
            pure img

    let loadTexture' url s o = do
            tex <- loadTexture url s o
            inc
            pure tex

    let loadMesh' name dir file s p = do
            mesh <- loadMesh name dir file s p
            inc
            pure mesh

    let loadText url = do
            res <- get url
            inc
            pure (res.response :: String)


    -- load images
    loadImage' "./image/loading.png"
    loadImage' "./image/title.png"
    loadImage' "./image/screenshade.png"

    -- load options
    response <- get "options.json"
    Options options <- case runExcept (readOptions response.response) of
        Left err -> throwError (error (show err))
        Right opt -> pure opt

    firebase <- liftEff $ initializeApp options.profile

    engine <- liftEff $ createEngine canvasGL true

    scene <- liftEff do
        sce <- createScene engine
        setFogMode fOGMODE_EXP sce
        setFogDensity options.fogDensity sce
        fogColor <- createColor3 (155.0 / 255.0) (181.0 / 255.0) (230.0 / 255.0)
        setFogColor fogColor sce
        setCollisionsEnabled true sce
        pure sce

    texture <- loadTexture' "./image/texture.png" scene defaultCreateTextureOptions
    alphaTexture <- loadTexture' "./image/alpha.png" scene defaultCreateTextureOptions

    loadTexture' "./alice/texture.png" scene defaultCreateTextureOptions             -- make sure the texture loaded
    playerMeshes <- loadMesh' "" "./alice/" "alice.babylon" scene pure
    loadText "./alice/cellShading.fragment.fx"
    loadText "./alice/cellShading.vertex.fx"
    loadText "./alice/outline.fragment.fx"
    loadText "./alice/outline.vertex.fx"
    loadText "./alice/alice.babylon.manifest"

    sounds <- loadSounds scene inc

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

        pure {
            options: Options options, engine, scene, skybox, cursor, materials, shadowMap, targetCamera, playerMeshes, sounds, firebase
        }



foreign import loadScript :: forall eff. String -> Eff (dom :: DOM | eff) Unit
