module Game.Cubbit.Sounds (Sounds, loadSounds, setMute, stopAllSounds) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Game.Cubbit.Types (Effects)
import Graphics.Babylon.Aff.Sound (loadSound)
import Graphics.Babylon.Sound (defaultCreateSoundOptions, setVolume, stop)
import Graphics.Babylon.Types (BABYLON, Scene, Sound)
import Prelude (($))

type Sounds = {
    yourNatural :: Sound,
    rye :: Sound,
    cleaning :: Sound,
    forestSound :: Sound,
    switchSound :: Sound,
    pickSound :: Sound,
    putSound :: Sound,
    stepSound :: Sound,
    warpSound :: Sound
}

loadSounds :: forall eff. Scene -> Aff (Effects eff) Sounds
loadSounds scene = do

    -- jingle
    yourNatural <- load "sound/Your_natural.mp3" true
    rye <- load "sound/ライ麦畑で朝食を.mp3" true
    cleaning <- load "sound/クリーニング・ストリーム.mp3" true

    -- environment
    forestSound <- load "sound/forest.mp3" true
    stepSound <- load "sound/step13a.mp3" true

    -- effects
    switchSound <- load "sound/tm2_switch001.mp3" false
    pickSound <- load "sound/bosu06.mp3" false
    putSound <- load "sound/bosu28_c.mp3" false
    warpSound <- load "sound/warp01.mp3" false

    pure $ {
        rye,
        yourNatural,
        cleaning,
        forestSound,
        switchSound,
        pickSound,
        putSound,
        stepSound,
        warpSound
    }

  where
    load url loop = loadSound url url scene defaultCreateSoundOptions { autoplay = false, loop = loop }

setMute :: forall eff. Boolean -> Sounds -> Eff (babylon :: BABYLON | eff) Unit
setMute mute sounds = do
    let go = setVolume (if mute then 0.0 else 1.0)
    go sounds.yourNatural
    go sounds.rye
    go sounds.cleaning
    go sounds.forestSound
    go sounds.stepSound
    go sounds.switchSound
    go sounds.pickSound
    go sounds.putSound
    go sounds.warpSound

stopAllSounds :: forall eff. Sounds -> Eff (babylon :: BABYLON | eff) Unit
stopAllSounds sounds = do
    stop sounds.yourNatural
    stop sounds.cleaning
    stop sounds.rye
    stop sounds.forestSound
    stop sounds.stepSound
    stop sounds.switchSound
    stop sounds.pickSound
    stop sounds.putSound
    stop sounds.warpSound