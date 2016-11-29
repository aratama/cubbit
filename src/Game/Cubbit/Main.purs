module Game.Cubbit.Main (main) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, when)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Ref (modifyRef, newRef)
import Data.Foldable (for_)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toMaybe, toNullable)
import Data.Unit (Unit, unit)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Constants (fogDensity)
import Game.Cubbit.Event (onKeyDown)
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
import Graphics.Babylon.Material (setAlpha, setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (createBox, meshToAbstractMesh, setInfiniteDistance)
import Graphics.Babylon.Scene (beginAnimation, createScene, fOGMODE_EXP, render, setActiveCamera, setActiveCameras, setCollisionsEnabled, setFogColor, setFogDensity, setFogMode)
import Graphics.Babylon.SceneLoader (importMesh)
import Graphics.Babylon.ShaderMaterial (createShaderMaterial, setColor3, setFloats, setTexture, setVector3, shaderMaterialToMaterial)
import Graphics.Babylon.ShadowGenerator (createShadowGenerator, getShadowMap, setBias, setUsePoissonSampling)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDiffuseTexture, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.TargetCamera (createTargetCamera, setSpeed, setTarget, targetCameraToCamera)
import Graphics.Babylon.Texture (createTexture, sKYBOX_MODE, setCoordinatesMode)
import Graphics.Babylon.Vector3 (createVector3)
import Graphics.Babylon.Viewport (createViewport)
import Graphics.Babylon.WaterMaterial (createWaterMaterial, setBumpTexture, addToRenderList, waterMaterialToMaterial, setWaveHeight, setWindForce)
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Prelude (negate, (#), ($), (+), (/), (<$>), (==))

shadowMapSize :: Int
shadowMapSize = 4096

skyBoxRenderingGruop :: Int
skyBoxRenderingGruop = 0

terrainRenderingGroup :: Int
terrainRenderingGroup = 1

collesionEnabledRange :: Int
collesionEnabledRange = 1

enableWaterMaterial :: Boolean
enableWaterMaterial = false

runApp :: forall eff. Canvas -> CanvasElement -> Eff (Effects eff) Unit
runApp canvasGL canvas2d = do

    engine <- createEngine canvasGL true

    -- create a basic BJS Scene object
    scene <- do
        sce <- createScene engine
        setFogMode fOGMODE_EXP sce
        setFogDensity fogDensity sce
        fogColor <- createColor3 (155.0 / 255.0) (181.0 / 255.0) (230.0 / 255.0)
        setFogColor fogColor sce
        setCollisionsEnabled true sce
        pure sce

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
        cameraPosition <- createVector3 3.0 17.0 3.0

        cam <- createFreeCamera "free-camera" cameraPosition scene
        -- setCheckCollisions true cam

        -- target the camera to scene origin
        cameraTarget <- createVector3 0.0 15.0 0.0
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
        lightDirection <- createVector3 0.4 (negate 0.8) 0.4
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
    materials <- do


        cellShadingMaterial <- createShaderMaterial "cellShading" scene "./alice/cellShading" {
            needAlphaBlending: false,
            needAlphaTesting: false,
            attributes: ["position", "uv", "normal", "matricesIndices", "matricesWeights"],
            uniforms: ["world", "viewProjection", "mBones"],
            samplers: ["textureSampler"],
            defines: []
        }
        lightPosition <- createVector3 0.0 20.0 (-10.0)
        lightColor <- createColor3 1.0 1.0 1.0
        cellShadingMaterialTexture <- createTexture "./alice/texture.png" scene
        setTexture "textureSampler" cellShadingMaterialTexture cellShadingMaterial
        setVector3 "vLightPosition" lightPosition cellShadingMaterial
        setFloats "ToonThresholds" [0.2, -0.45, -5.0, -5.0] cellShadingMaterial
        setFloats "ToonBrightnessLevels" [1.0, 0.9, 0.75, 0.75, 0.75] cellShadingMaterial
        setColor3 "vLightColor" lightColor cellShadingMaterial


        texture <- createTexture "texture.png" scene
        boxMat <- createStandardMaterial "grass-block" scene
        grassSpecular <- createColor3 0.0 0.0 0.0
        setSpecularColor grassSpecular boxMat
        -- setSpecularPower 0.0 boxMat
        setDiffuseTexture texture boxMat

        waterMaterial <- if enableWaterMaterial
            then do
                mat <- createWaterMaterial "water-block" scene
                tex <- createTexture "waterbump.png" scene
                setBumpTexture tex mat
                addToRenderList (meshToAbstractMesh skybox) mat
                setWaveHeight 0.0 mat
                setWindForce 1.0 mat
                pure (waterMaterialToMaterial mat)
            else do
                mat <- createStandardMaterial "water-block" scene
                color <- createColor3 (50.0 / 255.0) (50.0 / 255.0) (60.0 / 255.0)
                setDiffuseColor color mat
                -- setDiffuseTexture texture mat
                setAlpha 0.7 (standardMaterialToMaterial mat)
                pure (standardMaterialToMaterial mat)

        pure {
            boxMat: standardMaterialToMaterial boxMat,
            waterBoxMat: waterMaterial,
            cellShadingMaterial: shaderMaterialToMaterial cellShadingMaterial
        }

    initialTerrain <- emptyTerrain 0
    ref <- newRef $ State {
        mode: Move,
        terrain: initialTerrain,
        mousePosition: { x: 0, y: 0 },
        debugLayer: false,
        yaw: 0.0,
        pitch: 0.0,
        position: { x: 0.0, y: 20.0, z: 0.0 },
        velocity: { x: 0.0, y: 0.2, z: 0.0 },
        minimap: false,
        totalFrames: 0,
        playerMeshes: [],
        updateIndex: toNullable Nothing
    }

    initializeUI canvasGL canvas2d ref cursor freeCamera miniMapCamera scene materials

{-
    do
        spriteSize <- toNullable <$> Just <$> createSize 1280 720
        screenShade <- createTexture "screenshade.png" scene
        sprite2D <- createSprite2D screenShade { spriteSize }
        createScreenSpaceCanvas2D scene {
            id: "ScreenSpaceCanvas2D",
            children: [sprite2DToPrim2DBase sprite2D]
        }
-}
    let onSucc result = do
            for_ result \mesh -> void do
                p <- createVector3 0.5 13.0 0.5
                setPosition p mesh
                setRenderingGroupId 1 mesh
                setReceiveShadows true mesh
                skeleton <- getSkeleton mesh
                --setMaterial materials.cellShadingMaterial mesh
                beginAnimation skeleton 0 30 true 1.0 (toNullable Nothing) (toNullable Nothing) scene

                setMaterial materials.cellShadingMaterial mesh

            modifyRef ref \(State state) -> State state {
                playerMeshes = result
            }
            pure unit

    importMesh "" "./alice/" "alice.babylon" scene (toNullable (Just onSucc)) (toNullable Nothing) (toNullable Nothing)

    onKeyDown \e -> do
        when (e.keyCode == 32) do
            modifyRef ref \(State state) -> State state {
                velocity = {
                    x: state.velocity.x,
                    y: state.velocity.y + 0.1,
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
