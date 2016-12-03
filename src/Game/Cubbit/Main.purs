module Game.Cubbit.Main (main) where

import Control.Alternative (pure)
import Control.Bind (bind, when)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (errorShow, error)
import Control.Monad.Eff.Exception (error) as EXP
import Control.Monad.Eff.Ref (modifyRef, newRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toMaybe, toNullable)
import Data.Show (show)
import Data.Unit (Unit)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Constants (skyBoxRenderingGruop)
import Game.Cubbit.Event (onKeyDown, onKeyUp, focus)
import Game.Cubbit.Hud (initializeHud)
import Game.Cubbit.Materials (initializeMaterials)
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Option (readOptions)
import Game.Cubbit.Terrain (emptyTerrain)
import Game.Cubbit.Types (Effects, Mode(Move), State(State))
import Game.Cubbit.UI (initializeUI)
import Game.Cubbit.Update (update)
import Graphics.Babylon (Canvas, onDOMContentLoaded, querySelectorCanvas)
import Graphics.Babylon.AbstractMesh (setIsPickable, setIsVisible, getSkeleton, setMaterial, setPosition, setReceiveShadows, setRenderingGroupId)
import Graphics.Babylon.Camera (oRTHOGRAPHIC_CAMERA, setMode, setViewport, setOrthoLeft, setOrthoRight, setOrthoTop, setOrthoBottom, setMaxZ, setMinZ, setFOV)
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (createBox, meshToAbstractMesh, setInfiniteDistance)
import Graphics.Babylon.Scene (createScene, fOGMODE_EXP, render, setActiveCamera, setActiveCameras, setCollisionsEnabled, setFogColor, setFogDensity, setFogMode)
import Graphics.Babylon.SceneLoader.Aff (loadMesh)
import Graphics.Babylon.ShadowGenerator (createShadowGenerator, getShadowMap, setBias, setUsePoissonSampling)
import Graphics.Babylon.Sound (defaultCreateSoundOptions)
import Graphics.Babylon.Sound.Aff (loadSound)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.TargetCamera (createTargetCamera, setTarget, targetCameraToCamera)
import Graphics.Babylon.Texture (sKYBOX_MODE, setCoordinatesMode, defaultCreateTextureOptions)
import Graphics.Babylon.Texture.Aff (loadTexture)
import Graphics.Babylon.Vector3 (createVector3)
import Graphics.Babylon.Viewport (createViewport)
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Halogen.Util (runHalogenAff)
import Network.HTTP.Affjax (get)
import Prelude (negate, (#), ($), (+), (/), (<$>), (==), void)


runApp :: forall eff. Canvas -> CanvasElement -> Eff (Effects eff) Unit
runApp canvasGL canvas2d = void $ runAff errorShow pure do

    -- load options
    response <- get "options.json"
    options <- case runExcept (readOptions response.response) of
        Left err -> throwError (EXP.error (show err))
        Right opt -> pure opt

    engine <- liftEff $ createEngine canvasGL true

    scene <- liftEff do
        sce <- createScene engine
        setFogMode fOGMODE_EXP sce
        setFogDensity options.fogDensity sce
        fogColor <- createColor3 (155.0 / 255.0) (181.0 / 255.0) (230.0 / 255.0)
        setFogColor fogColor sce
        setCollisionsEnabled true sce
        pure sce

    -- load resources
    texture <- loadTexture "./texture.png" scene defaultCreateTextureOptions
    alphaTexture <- loadTexture "./alpha.png" scene defaultCreateTextureOptions
    loadTexture "./alice/texture.png" scene defaultCreateTextureOptions             -- make sure the texture loaded
    playerMeshes <- loadMesh "" "./alice/" "alice.babylon" scene pure
    forestSound <- loadSound "forest.mp3" "forest.mp3" scene defaultCreateSoundOptions { autoplay = true, loop = true }



    liftEff do





        -- initialize scene

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

        targetCamera <- do
            cameraPosition <- createVector3 10.0 20.0 (negate 10.0)

            cam <- createTargetCamera "target-camera" cameraPosition scene
            -- setCheckCollisions true cam

            -- target the camera to scene origin
            cameraTarget <- createVector3 0.0 8.0 0.0
            setTarget cameraTarget cam
            setMaxZ options.cameraMaxZ (targetCameraToCamera cam)
            setMinZ options.cameraMinZ (targetCameraToCamera cam)
            setFOV options.cameraFOV (targetCameraToCamera cam)
            pure cam


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
        materials <- initializeMaterials scene skybox texture alphaTexture options

        -- initialize game state
        initialTerrain <- emptyTerrain 0
        ref <- newRef $ State {
            mode: Move,
            terrain: initialTerrain,
            mousePosition: { x: 0, y: 0 },
            debugLayer: false,

            cameraPosition: { x: 10.0, y: 20.0, z: negate 10.0 },
            cameraTarget: { x: 0.5, y: 11.0, z: 0.5 },
            cameraYaw: 0.0,
            cameraPitch: 0.7,
            cameraRange: 5.0,
            firstPersonView: false,
            firstPersonViewPitch: 0.0,

            position: { x: 0.5, y: 10.0, z: 0.5 },
            velocity: { x: 0.0, y: 0.0, z: 0.0 },
            playerRotation: 0.5,
            playerPitch: 0.0,
            minimap: false,
            totalFrames: 0,
            playerMeshes: playerMeshes,
            updateIndex: toNullable Nothing,

            wKey: false,
            sKey: false,
            aKey: false,
            dKey: false,
            qKey: false,
            eKey: false,
            rKey: false,
            fKey: false,
            tKey: false,
            gKey: false,

            animation: ""
        }

        initializeUI canvasGL canvas2d ref cursor targetCamera miniMapCamera scene materials options


        -- initialize player charactor mesh
        for_ playerMeshes \mesh -> void do
            p <- createVector3 0.5 13.0 0.5
            setPosition p mesh
            setRenderingGroupId 1 mesh
            setReceiveShadows true mesh
            skeleton <- getSkeleton mesh
            setMaterial materials.cellShadingMaterial mesh
            --playAnimation "Stand" ref
            -- beginAnimation skeleton 0 30 true 1.0 (toNullable Nothing) (toNullable Nothing) scene


        -- TODO: handle key events
        onKeyDown \e -> do
            when (e.keyCode == 32) do
                modifyRef ref \(State state) -> State state {
                    velocity = state.velocity { y = state.velocity.y + options.jumpVelocity }
                }

            when (e.keyCode == 87) do -- w
                modifyRef ref \(State state) -> State state { wKey = true }
            when (e.keyCode == 83) do -- s
                modifyRef ref \(State state) -> State state { sKey = true }
            when (e.keyCode == 65) do -- a
                modifyRef ref \(State state) -> State state { aKey = true }
            when (e.keyCode == 68) do -- d
                modifyRef ref \(State state) -> State state { dKey = true }
            when (e.keyCode == 82) do -- r
                modifyRef ref \(State state) -> State state { rKey = true }
            when (e.keyCode == 70) do -- f
                modifyRef ref \(State state) -> State state { fKey = true }
            when (e.keyCode == 81) do -- q
                modifyRef ref \(State state) -> State state { qKey = true }
            when (e.keyCode == 69) do -- e
                modifyRef ref \(State state) -> State state { eKey = true }
            when (e.keyCode == 84) do -- t
                modifyRef ref \(State state) -> State state { tKey = true }
            when (e.keyCode == 71) do -- g
                modifyRef ref \(State state) -> State state { gKey = true }

        onKeyUp \e -> do
            when (e.keyCode == 87) do -- w
                modifyRef ref \(State state) -> State state { wKey = false }
            when (e.keyCode == 83) do -- s
                modifyRef ref \(State state) -> State state { sKey = false }
            when (e.keyCode == 65) do -- a
                modifyRef ref \(State state) -> State state { aKey = false }
            when (e.keyCode == 68) do -- d
                modifyRef ref \(State state) -> State state { dKey = false }
            when (e.keyCode == 82) do -- r
                modifyRef ref \(State state) -> State state { rKey = false }
            when (e.keyCode == 70) do -- f
                modifyRef ref \(State state) -> State state { fKey = false }
            when (e.keyCode == 81) do -- q
                modifyRef ref \(State state) -> State state { qKey = false }
            when (e.keyCode == 69) do -- e
                modifyRef ref \(State state) -> State state { eKey = false }
            when (e.keyCode == 84) do -- t
                modifyRef ref \(State state) -> State state { tKey = false }
            when (e.keyCode == 71) do -- g
                modifyRef ref \(State state) -> State state { gKey = false }
        -- focus
        focus "renderCanvas"

        -- load initial chunks
        do
            let initialWorldSize = options.initialWorldSize
            forE (-initialWorldSize) initialWorldSize \x -> do
                forE (-initialWorldSize) initialWorldSize \z -> void do
                    let index = chunkIndex x 0 z
                    createChunkMesh ref materials scene index


        -- start game loop
        engine # runRenderLoop do
            update ref scene materials shadowMap cursor targetCamera options skybox
            render scene

        hideLoading

main :: forall eff. Eff (Effects eff) Unit
main = runHalogenAff do
    driver <- initializeHud
    liftEff do
        canvasM <- toMaybe <$> querySelectorCanvas "#renderCanvas"
        canvas2dM <- getCanvasElementById "canvas2d"
        case canvasM, canvas2dM of
            Just canvasGL, Just canvas2d -> runApp canvasGL canvas2d
            _, _ -> error "canvasGL not found"



foreign import hideLoading :: forall eff. Eff eff Unit
