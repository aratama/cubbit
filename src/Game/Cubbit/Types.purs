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
import Data.Set (Set)
import Game.Cubbit.BlockIndex (BlockIndex)
import Game.Cubbit.BlockType (BlockType)
import Game.Cubbit.Config (Config)
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Terrain (Terrain)
import Game.Cubbit.Vec (Vec)
import Graphics.Babylon.Types (BABYLON, Canvas, Sound)
import Graphics.Cannon.Type (CANNON, World)
import Graphics.Canvas (CANVAS)
import Network.HTTP.Affjax (AJAX)
import Web.Firebase (FIREBASE, Firebase)

type CoreEffects eff = (
    canvas :: CANVAS,
    now :: NOW,
    console :: CONSOLE,
    babylon :: BABYLON,
    ref :: REF,
    ajax :: AJAX,
    storage :: STORAGE,
    timer :: TIMER,
    cannon :: CANNON,
    firebase :: FIREBASE | eff)

type Effects eff =  CoreEffects (
    dom :: DOM,
    err :: EXCEPTION,
    avar :: AVAR | eff)

data Mode = Move | Put BlockType | Remove


type TitleSceneState = {
    position :: Number
}

type PlayingSceneState = {

    gameMode :: GameMode,

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
    jumpable :: Boolean,

    cursorPosition :: BlockIndex,
    centerPanelVisible :: Boolean,
    life :: Int,
    maxLife :: Int
}

type ModeSelectionSceneState = {}

data SceneState = TitleSceneState TitleSceneState
                | ModeSelectionSceneState ModeSelectionSceneState
                | PlayingSceneState PlayingSceneState

data GameMode = SinglePlayerMode | MultiplayerMode


data ResourceProgress = Loading Int | Complete Resources

newtype State = State {
    canvas :: Canvas,
    config :: Config,
    res :: ResourceProgress,

    configVisible :: Boolean,


    sceneState :: SceneState,
    nextScene ::Maybe SceneState,

    -- world
    terrain :: Terrain,
    skyboxRotation :: Number,
    updateIndex :: Nullable ForeachIndex,

    world :: World,

    firebase :: Firebase,

    -- camera
    cameraPosition :: Vec,
    cameraTarget :: Vec,

    -- ui
    totalFrames :: Int,
    minimap :: Boolean,
    debugLayer :: Boolean,
    mousePosition :: { x :: Int, y :: Int },
    keys :: Set String,
    bgm :: Maybe Sound,
    nextBGM :: Maybe Sound,
    volume :: Number,

    niconico :: Boolean
}

foreign import data ForeachIndex :: *

derive instance eqMode :: Eq Mode
