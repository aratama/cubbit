module Game.Cubbit.Types where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.WebStorage (STORAGE)
import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Game.Cubbit.BlockIndex (BlockIndex)
import Game.Cubbit.BlockType (BlockType(..))
import Game.Cubbit.Terrain (Terrain)
import Game.Cubbit.Vec (Vec)
import Graphics.Babylon.Types (AbstractMesh, BABYLON, Material, Sound)
import Graphics.Canvas (CANVAS)
import Network.HTTP.Affjax (AJAX)

type CoreEffects eff = (
    canvas :: CANVAS,
    now :: NOW,
    console :: CONSOLE,
    babylon :: BABYLON,
    ref :: REF,
    ajax :: AJAX,
    storage :: STORAGE,
    timer :: TIMER | eff)

type Effects eff =  CoreEffects (
    dom :: DOM,
    err :: EXCEPTION,
    avar :: AVAR | eff)

data Mode = Move | Put BlockType | Remove


type PlayingSceneState = {
    playerMeshes :: Array AbstractMesh,
    cameraYaw :: Number,
    cameraPitch :: Number,
    cameraRange :: Number,
    firstPersonViewPitch :: Number,
    firstPersonView :: Boolean,
    position :: Vec,
    velocity :: Vec,
    playerRotation :: Number,
    playerPitch :: Number,
    animation :: String,
    mode :: Mode,
    landing :: Int,

    cursorPosition :: BlockIndex,
    centerPanelVisible :: Boolean,
    life :: Int,
    maxLife :: Int
}

data SceneState = TitleSceneState | PlayingSceneState PlayingSceneState

newtype State = State {

    -- TODO: Remove them

    firstPersonViewPitch :: Number,
    position :: Vec,
    velocity :: Vec,
    playerRotation :: Number,
    playerPitch :: Number,
    mode :: Mode,
    landing :: Int,

    sceneState :: SceneState,
    nextScene ::Maybe SceneState,

    mute :: Boolean,

    -- world
    terrain :: Terrain,
    skyboxRotation :: Number,
    updateIndex :: Nullable ForeachIndex,

    -- camera
    cameraPosition :: Vec,
    cameraTarget :: Vec,

    -- ui
    totalFrames :: Int,
    minimap :: Boolean,
    debugLayer :: Boolean,
    mousePosition :: { x :: Int, y :: Int },
    spaceKey :: Boolean,
    wKey :: Boolean,
    sKey :: Boolean,
    aKey :: Boolean,
    dKey :: Boolean,
    qKey :: Boolean,
    eKey :: Boolean,
    rKey :: Boolean,
    fKey :: Boolean,
    tKey :: Boolean,
    gKey :: Boolean
}







foreign import data ForeachIndex :: *


derive instance eqMode :: Eq Mode
