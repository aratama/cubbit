module Game.Cubbit.Option (Options(Options), readOptions) where

import Data.Foreign (F, Foreign)
import Data.Foreign.Generic (readGeneric, defaultOptions)
import Data.Generic (class Generic)

newtype Options = Options {
    eyeHeight :: Number,
    gravity :: Number,
    loadDistance :: Int,
    fogDensity :: Number,
    maximumLoadedChunks :: Int,
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

derive instance genericOptions :: Generic Options

readOptions :: Foreign -> F Options
readOptions = readGeneric defaultOptions { unwrapNewtypes = true }
