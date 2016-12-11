module Game.Cubbit.Sounds (Sounds, loadSounds) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Game.Cubbit.Types (Effects)
import Graphics.Babylon.Aff.Sound (loadSound)
import Graphics.Babylon.Sound (defaultCreateSoundOptions)
import Graphics.Babylon.Types (Scene, Sound)
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