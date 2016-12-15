module Game.Cubbit.Sounds (Sounds, loadSounds, setMute) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Game.Cubbit.Types (Effects)
import Graphics.Babylon.Aff.Sound (loadSound)
import Graphics.Babylon.Sound (defaultCreateSoundOptions, setVolume)
import Graphics.Babylon.Types (BABYLON, Scene, Sound)
import Prelude (($))

type Sounds = {
    forestSound :: Sound,
    switchSound :: Sound,
    pickSound :: Sound,
    putSound :: Sound,
    stepSound :: Sound,
    warpSound :: Sound
}

loadSounds :: forall eff. Scene -> Aff (Effects eff) Sounds
loadSounds scene = do

    -- environment
    forestSound <- load "sound/forest.mp3" true
    stepSound <- load "sound/step13a.mp3" true

    -- effects
    switchSound <- load "sound/tm2_switch001.mp3" false
    pickSound <- load "sound/bosu06.mp3" false
    putSound <- load "sound/bosu28_c.mp3" false
    warpSound <- load "sound/warp01.mp3" false
    pure $ { forestSound, switchSound, pickSound, putSound, stepSound, warpSound }

  where
    load url loop = loadSound url url scene defaultCreateSoundOptions { autoplay = false, loop = loop }

setMute :: forall eff. Boolean -> Sounds -> Eff (babylon :: BABYLON | eff) Unit
setMute mute sounds = do
    let go = setVolume (if mute then 0.0 else 1.0)
    go sounds.forestSound
    go sounds.stepSound
    go sounds.switchSound
    go sounds.pickSound
    go sounds.putSound
    go sounds.warpSound
