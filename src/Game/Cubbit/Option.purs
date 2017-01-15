module Game.Cubbit.Option (Options(Options)) where

import Data.Foreign.Class (class IsForeign)
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
    profile :: Profile,
    wheelSpeed :: Number
}

derive instance genericOptions :: Generic Options _

instance isForeignOptions :: IsForeign Options where
    read = readGeneric defaultOptions { unwrapSingleConstructors = true }
