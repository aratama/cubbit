module Game.Cubbit.Option (Options(Options), readOptions) where

import Data.Foreign (F, Foreign)
import Data.Foreign.Generic (readGeneric, defaultOptions)
import Data.Generic.Rep (class Generic)
import Web.Firebase (Profile)

newtype Options = Options {
    eyeHeight :: Number,
    gravity :: Number,
    fogDensity :: Number,
    maximumLoadedChunks :: Int,
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
    landingDuration :: Int,
    blockPickingMaxDistance :: Int,
    profile :: Profile
}

derive instance genericOptions :: Generic Options _

readOptions :: Foreign -> F Options
readOptions = readGeneric defaultOptions { unwrapSingleConstructors = true }
