module Game.Cubbit.Option (Options, readOptions) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (readProp)

type Options = {
    loadDistance :: Int,
    fogDensity :: Number,
    maximumLoadedChunks :: Int,
    shadowDisplayRange :: Int,
    shadowMapSize :: Int,
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
    loadDistance <- readProp "loadDistance" value
    fogDensity <- readProp "fogDensity" value
    maximumLoadedChunks <- readProp "maximumLoadedChunks" value
    shadowDisplayRange <- readProp "shadowDisplayRange" value
    shadowMapSize <- readProp "shadowMapSize" value
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
        loadDistance,
        fogDensity,
        maximumLoadedChunks,
        shadowDisplayRange,
        shadowMapSize,
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

