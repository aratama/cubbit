module Game.Cubbit.Option (Options, readOptions) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (readProp)

type Options = {
    eyeHeight :: Number,
    gravity :: Number,
    loadDistance :: Int,
    fogDensity :: Number,
    maximumLoadedChunks :: Int,
    vertexColorEnabled :: Boolean,
    shadowEnabled :: Boolean,
    shadowDisplayRange :: Int,
    shadowMapSize :: Int,
    skyboxRotationSpeed :: Number,
    enableWaterMaterial :: Boolean,
    chunkUnloadSpeed :: Int,
    jumpVelocity :: Number,
    initialWorldSize :: Int,
    moveSpeed :: Number,
    cameraTargetSpeed :: Number,
    cameraRotationSpeed :: Number,
    cameraZoomSpeed :: Number,
    cameraMaxZ :: Number,
    cameraMinZ :: Number,
    cameraFOV :: Number,
    cameraMinimumRange :: Number,
    cameraMaximumRange :: Number,
    cameraHorizontalSensitivity :: Number,
    cameraVertialSensitivity :: Number,
    pointerHorizontalSensitivity :: Number,
    pointerVerticalSensitivity :: Number,
    landingVelocityLimit :: Number,
    landingDuration :: Int
}

readOptions :: Foreign -> F Options
readOptions value = do
    eyeHeight <- readProp "eyeHeight" value
    gravity <- readProp "gravity" value
    loadDistance <- readProp "loadDistance" value
    fogDensity <- readProp "fogDensity" value
    maximumLoadedChunks <- readProp "maximumLoadedChunks" value
    vertexColorEnabled <- readProp "vertexColorEnabled" value
    shadowEnabled <- readProp "shadowEnabled" value
    shadowDisplayRange <- readProp "shadowDisplayRange" value
    shadowMapSize <- readProp "shadowMapSize" value
    skyboxRotationSpeed <- readProp "skyboxRotationSpeed" value
    enableWaterMaterial <- readProp "enableWaterMaterial" value
    chunkUnloadSpeed <- readProp "chunkUnloadSpeed" value
    jumpVelocity <- readProp "jumpVelocity" value
    initialWorldSize <- readProp "initialWorldSize" value
    moveSpeed <- readProp "moveSpeed" value
    cameraTargetSpeed <- readProp "cameraTargetSpeed" value
    cameraRotationSpeed <- readProp "cameraRotationSpeed" value
    cameraZoomSpeed <- readProp "cameraZoomSpeed" value
    cameraMinZ <- readProp "cameraMinZ" value
    cameraMaxZ <- readProp "cameraMaxZ" value
    cameraFOV <- readProp "cameraFOV" value
    cameraMinimumRange <- readProp "cameraMinimumRange" value
    cameraMaximumRange <- readProp "cameraMaximumRange" value
    cameraHorizontalSensitivity <- readProp "cameraHorizontalSensitivity" value
    cameraVertialSensitivity <- readProp "cameraVertialSensitivity" value
    pointerHorizontalSensitivity <- readProp "pointerHorizontalSensitivity" value
    pointerVerticalSensitivity <- readProp "pointerVerticalSensitivity" value
    landingVelocityLimit <- readProp "landingVelocityLimit" value
    landingDuration <- readProp "landingDuration" value
    pure {
        eyeHeight,
        gravity,
        loadDistance,
        fogDensity,
        maximumLoadedChunks,
        vertexColorEnabled,
        shadowDisplayRange,
        shadowEnabled,
        shadowMapSize,
        skyboxRotationSpeed,
        enableWaterMaterial,
        chunkUnloadSpeed,
        jumpVelocity,
        initialWorldSize,
        moveSpeed,
        cameraTargetSpeed,
        cameraRotationSpeed,
        cameraZoomSpeed,
        cameraMinZ,
        cameraMaxZ,
        cameraFOV,
        cameraMinimumRange,
        cameraMaximumRange,
        cameraHorizontalSensitivity,
        cameraVertialSensitivity,
        pointerHorizontalSensitivity,
        pointerVerticalSensitivity,
        landingVelocityLimit,
        landingDuration
    }

