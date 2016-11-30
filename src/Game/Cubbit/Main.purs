module Game.Cubbit.Main (main) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, when)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (errorShow, error)
import Control.Monad.Eff.Ref (modifyRef, newRef)
import Data.Foldable (for_)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toMaybe, toNullable)
import Data.Unit (Unit, unit)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Constants (fogDensity, shadowMapSize, skyBoxRenderingGruop)
import Game.Cubbit.Event (onKeyDown)
import Game.Cubbit.Materials (initializeMaterials)
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Terrain (emptyTerrain)
import Game.Cubbit.Types (Effects, Mode(..), State(State))
import Game.Cubbit.UI (initializeUI)
import Game.Cubbit.Update (update)
import Graphics.Babylon (Canvas, onDOMContentLoaded, querySelectorCanvas)
import Graphics.Babylon.AbstractMesh (setIsPickable, setIsVisible, getSkeleton, setMaterial, setPosition, setReceiveShadows, setRenderingGroupId)
import Graphics.Babylon.Camera (oRTHOGRAPHIC_CAMERA, setMode, setViewport, setOrthoLeft, setOrthoRight, setOrthoTop, setOrthoBottom)
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.FreeCamera (attachControl, createFreeCamera, freeCameraToCamera, freeCameraToTargetCamera)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (createBox, meshToAbstractMesh, setInfiniteDistance)
import Graphics.Babylon.Scene (beginAnimation, createScene, fOGMODE_EXP, render, setActiveCamera, setActiveCameras, setCollisionsEnabled, setFogColor, setFogDensity, setFogMode)
import Graphics.Babylon.SceneLoader (importMesh)
import Graphics.Babylon.SceneLoader.Aff (loadMesh)
import Graphics.Babylon.ShadowGenerator (createShadowGenerator, getShadowMap, setBias, setUsePoissonSampling)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.TargetCamera (createTargetCamera, setSpeed, setTarget, targetCameraToCamera)
import Graphics.Babylon.Texture (sKYBOX_MODE, setCoordinatesMode, defaultCreateTextureOptions)
import Graphics.Babylon.Texture.Aff (loadTexture)
import Graphics.Babylon.Vector3 (createVector3)
import Graphics.Babylon.Viewport (createViewport)
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Prelude (negate, (#), ($), (+), (/), (<$>), (==), void)


runApp :: forall eff. Canvas -> CanvasElement -> Eff (Effects eff) Unit
runApp canvasGL canvas2d = void $ runAff errorShow pure do

    engine <- liftEff $ createEngine canvasGL true

    scene <- liftEff do
        sce <- createScene engine
        setFogMode fOGMODE_EXP sce
        setFogDensity fogDensity sce
        fogColor <- createColor3 (155.0 / 255.0) (181.0 / 255.0) (230.0 / 255.0)
        setFogColor fogColor sce
        setCollisionsEnabled true sce
        pure sce

    -- load resources
    texture <- loadTexture "./texture.png" scene defaultCreateTextureOptions
    alphaTexture <- loadTexture "./alpha.png" scene defaultCreateTextureOptions
    loadTexture "./alice/texture.png" scene defaultCreateTextureOptions             -- make sure the texture loaded
    playerMeshes <- loadMesh "" "./alice/" "alice.babylon" scene pure

    -- initialize scene
    liftEff do
        miniMapCamera <- do
            let minimapScale = 200.0
            position <- createVector3 0.0 30.0 0.0
            cam <- createTargetCamera "minimap-camera" position scene
            target <- createVector3 0.0 0.0 0.0
            setTarget target cam
            setMode oRTHOGRAPHIC_CAMERA (targetCameraToCamera cam)
            setOrthoLeft (-minimapScale) (targetCameraToCamera cam)
            setOrthoRight minimapScale (targetCameraToCamera cam)
            setOrthoTop minimapScale (targetCameraToCamera cam)
            setOrthoBottom (-minimapScale) (targetCameraToCamera cam)
            viewport <- createViewport 0.75 0.65 0.24 0.32
            setViewport viewport (targetCameraToCamera cam)
            pure cam

        freeCamera <- do
            cameraPosition <- createVector3 10.0 20.0 10.0

            cam <- createFreeCamera "free-camera" cameraPosition scene
            -- setCheckCollisions true cam

            -- target the camera to scene origin
            cameraTarget <- createVector3 0.0 8.0 0.0
            setTarget cameraTarget (freeCameraToTargetCamera cam)

            -- attach the camera to the canvasGL
            attachControl canvasGL false cam
            setSpeed 0.3 (freeCameraToTargetCamera cam)

            pure cam

        setActiveCameras [freeCameraToCamera freeCamera] scene
        setActiveCamera (freeCameraToCamera freeCamera) scene

        do
            hemiPosition <- createVector3 0.0 1.0 0.0
            hemiLight <- createHemisphericLight "Hemi0" hemiPosition scene
            diffuse <- createColor3 0.6 0.6 0.6
            setDiffuse diffuse (hemisphericLightToLight hemiLight)

        shadowMap <- do
            -- create a basic light, aiming 0,1,0 - meaning, to the sky
            lightDirection <- createVector3 0.0 (negate 1.0) 0.0
            light <- createDirectionalLight "light1" lightDirection scene
            dirColor <- createColor3 0.8 0.8 0.8
            setDiffuse dirColor (directionalLightToLight light)

            -- shadow
            shadowGenerator <- createShadowGenerator shadowMapSize light
            setBias 0.000005 shadowGenerator
            setUsePoissonSampling true shadowGenerator
            getShadowMap shadowGenerator

        cursor <- do
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
        skybox <- do
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
        materials <- initializeMaterials scene skybox texture alphaTexture

        -- initialize game state
        initialTerrain <- emptyTerrain 0
        ref <- newRef $ State {
            mode: Move,
            terrain: initialTerrain,
            mousePosition: { x: 0, y: 0 },
            debugLayer: false,
            yaw: 0.0,
            pitch: 0.0,
            position: { x: 0.5, y: 10.0, z: 0.5 },
            velocity: { x: 0.0, y: 0.2, z: 0.0 },
            minimap: false,
            totalFrames: 0,
            playerMeshes: playerMeshes,
            updateIndex: toNullable Nothing
        }

        initializeUI canvasGL canvas2d ref cursor freeCamera miniMapCamera scene materials


        -- initialize player charactor mesh
        for_ playerMeshes \mesh -> void do
            p <- createVector3 0.5 13.0 0.5
            setPosition p mesh
            setRenderingGroupId 1 mesh
            setReceiveShadows true mesh
            skeleton <- getSkeleton mesh
            --setMaterial materials.cellShadingMaterial mesh
            beginAnimation skeleton 0 30 true 1.0 (toNullable Nothing) (toNullable Nothing) scene
            setMaterial materials.cellShadingMaterial mesh

        -- TODO: handle key events
        onKeyDown \e -> do
            when (e.keyCode == 32) do
                modifyRef ref \(State state) -> State state {
                    velocity = {
                        x: state.velocity.x,
                        y: state.velocity.y + 0.15,
                        z: state.velocity.z
                    }
                }

        -- load initial chunks
        do
            let initialWorldSize = 5
            forE (-initialWorldSize) initialWorldSize \x -> do
                forE (-initialWorldSize) initialWorldSize \z -> void do
                    let index = chunkIndex x 0 z
                    createChunkMesh ref materials scene index


        -- start game loop
        engine # runRenderLoop do
            update ref scene materials shadowMap cursor freeCamera
            render scene

main :: forall eff. Eff (Effects eff) Unit
main = onDOMContentLoaded do
    canvasM <- toMaybe <$> querySelectorCanvas "#renderCanvas"
    canvas2dM <- getCanvasElementById "canvas2d"
    case canvasM, canvas2dM of
        Just canvasGL, Just canvas2d -> runApp canvasGL canvas2d
        _, _ -> error "canvasGL not found"
