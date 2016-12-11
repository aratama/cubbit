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
    forestSound <- loadSound "forest.mp3" "sound/forest.mp3" scene defaultCreateSoundOptions { autoplay = false, loop = true }
    switchSound <- loadSound "tm2_switch001.mp3" "sound/tm2_switch001.mp3" scene defaultCreateSoundOptions { autoplay = false, loop = false }
    pickSound <- loadSound "bosu06.mp3" "sound/bosu06.mp3" scene defaultCreateSoundOptions { autoplay = false, loop = false }
    putSound <- loadSound "bosu28_c.mp3" "sound/bosu28_c.mp3" scene defaultCreateSoundOptions { autoplay = false, loop = false }
    stepSound <- loadSound "step13a.mp3" "sound/step13a.mp3" scene defaultCreateSoundOptions { autoplay = false, loop = false }
    warpSound <- loadSound "warp01.mp3" "sound/warp01.mp3" scene defaultCreateSoundOptions { autoplay = false, loop = false }
    pure $ { forestSound, switchSound, pickSound, putSound, stepSound, warpSound }
