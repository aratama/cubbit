module Game.Cubbit.Main (main) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef)
import Control.MonadPlus (guard)
import Data.Foldable (for_)
import Data.List (List(..), (..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (toMaybe, toNullable)
import Data.Ring (negate)
import Data.Unit (Unit, unit)
import Game.Cubbit.Chunk (Chunk(..), ChunkWithMesh, MeshLoadingState(..))
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Event (onKeyDown)
import Game.Cubbit.Generation (createBlockMap)
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Terrain (Terrain(..), emptyTerrain, insertChunk, lookupChunk)
import Game.Cubbit.Types (Effects, Mode(..), State(State))
import Game.Cubbit.UI (initializeUI)
import Game.Cubbit.Update (update)
import Graphics.Babylon (Canvas, onDOMContentLoaded, querySelectorCanvas)
import Graphics.Babylon.AbstractMesh (getSkeleton, onCollisionPositionChangeObservable, setPosition, setReceiveShadows, setRenderingGroupId)
import Graphics.Babylon.AbstractMesh (setIsPickable, setIsVisible, setCheckCollisions) as AbstractMesh
import Graphics.Babylon.Camera (oRTHOGRAPHIC_CAMERA, setMode, setViewport, setOrthoLeft, setOrthoRight, setOrthoTop, setOrthoBottom)
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.FreeCamera (attachControl, createFreeCamera, freeCameraToCamera, freeCameraToTargetCamera, setCheckCollisions)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (createBox, meshToAbstractMesh, setInfiniteDistance, setMaterial)
import Graphics.Babylon.Observable (add) as Observable
import Graphics.Babylon.Scene (beginAnimation, createScene, fOGMODE_EXP, render, setActiveCamera, setActiveCameras, setCollisionsEnabled, setFogColor, setFogDensity, setFogEnd, setFogMode, setFogStart)
import Graphics.Babylon.SceneLoader (importMesh)
import Graphics.Babylon.ScreenSpaceCanvas2D (createScreenSpaceCanvas2D)
import Graphics.Babylon.ShadowGenerator (createShadowGenerator, getShadowMap, setBias, setUsePoissonSampling)
import Graphics.Babylon.Size (createSize)
import Graphics.Babylon.Sprite2D (createSprite2D, sprite2DToPrim2DBase)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDiffuseTexture, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.TargetCamera (createTargetCamera, setSpeed, setTarget, targetCameraToCamera)
import Graphics.Babylon.Texture (createTexture, sKYBOX_MODE, setCoordinatesMode)
import Graphics.Babylon.Types (AbstractMesh)
import Graphics.Babylon.Vector3 (createVector3, runVector3)
import Graphics.Babylon.Viewport (createViewport)
import Graphics.Babylon.WaterMaterial (createWaterMaterial, setBumpTexture, addToRenderList, waterMaterialToMaterial, setWaveHeight, setWindForce)
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Prelude ((#), ($), (<$>), (==), (-), (+), negate, (<), (>), (&&), (<>), show, (/))

shadowMapSize :: Int
shadowMapSize = 4096

loadDistance :: Int
loadDistance = 8

unloadDistance :: Int
unloadDistance = 8

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
        setFogDensity 0.01 sce
        setFogStart 250.0 sce
        setFogEnd 1000.0 sce
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

    camera <- do
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

    setActiveCameras [freeCameraToCamera camera] scene
    setActiveCamera (freeCameraToCamera camera) scene

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
        AbstractMesh.setIsPickable false (meshToAbstractMesh cursorbox)
        AbstractMesh.setIsVisible false (meshToAbstractMesh cursorbox)

        mat <- createStandardMaterial "cursormat" scene
        setWireframe true (standardMaterialToMaterial mat)
        setZOffset (negate 0.01) (standardMaterialToMaterial mat)
        setMaterial (standardMaterialToMaterial mat) cursorbox
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
        setMaterial (standardMaterialToMaterial skyboxMaterial) skyboxMesh
        setInfiniteDistance true skyboxMesh
        pure skyboxMesh

    -- prepare materials
    materials <- do
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
                setDiffuseTexture texture mat
                pure (standardMaterialToMaterial mat)

        pure { boxMat: standardMaterialToMaterial boxMat, waterBoxMat: waterMaterial }

    terrain <- emptyTerrain 0
    ref <- newRef $ State {
        mode: Move,
        terrain: terrain,
        mousePosition: { x: 0, y: 0 },
        debugLayer: false,
        yaw: 0.0,
        pitch: 0.0,
        position: { x: 0.0, y: 20.0, z: 0.0 },
        velocity: { x: 0.0, y: 0.2, z: 0.0 },
        minimap: false,
        totalFrames: 0,
        updateList: Nil,
        playerMeshes: [],
        updateIndex: toNullable Nothing,
        pickableMeshList: Nil
    }

    initializeUI canvasGL canvas2d ref cursor camera miniMapCamera scene materials

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
                beginAnimation skeleton 0 30 true 1.0 (toNullable Nothing) (toNullable Nothing) scene
            modifyRef ref \(State state) -> State state {
                playerMeshes = result
            }
            pure unit

    importMesh "" "/alice/" "alice.babylon" scene (toNullable (Just onSucc)) (toNullable Nothing) (toNullable Nothing)

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
        let indices = do
                x <- (- 2) .. 2
                y <- (- 2) .. 2
                z <- (- 2) .. 2
                pure (chunkIndex x y z)
        for_ indices \index -> do
            State state@{ terrain: Terrain terrain } <- readRef ref
            let boxMap = createBlockMap terrain.noise index
            let result = { blocks: boxMap, standardMaterialMesh: MeshNotLoaded }
            insertChunk result state.terrain

        for_ indices \index -> do
            createChunkMesh ref materials scene index


    -- start game loop
    engine # runRenderLoop do
        update ref scene materials shadowMap cursor camera
        render scene

main :: forall eff. Eff (Effects eff) Unit
main = onDOMContentLoaded do
    canvasM <- toMaybe <$> querySelectorCanvas "#renderCanvas"
    canvas2dM <- getCanvasElementById "canvas2d"
    case canvasM, canvas2dM of
        Just canvasGL, Just canvas2d -> runApp canvasGL canvas2d
        _, _ -> error "canvasGL not found"
