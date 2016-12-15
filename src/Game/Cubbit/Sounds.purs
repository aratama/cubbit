module Game.Cubbit.Sounds (Sounds, loadSounds, setMute, stopAllSounds, setBGMVolume) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Traversable (for_)
import Data.Unit (Unit)
import Game.Cubbit.Types (Effects)
import Graphics.Babylon.Aff.Sound (loadSound)
import Graphics.Babylon.Sound (defaultCreateSoundOptions, setVolume, stop)
import Graphics.Babylon.Types (BABYLON, Scene, Sound)
import Prelude (($), (<>))

type Sounds = {
    yourNatural :: Sound,
    rye :: Sound,
    cleaning :: Sound,
    forestSound :: Sound,
    switchSound :: Sound,
    pickSound :: Sound,
    putSound :: Sound,
    stepSound :: Sound,
    warpSound :: Sound,

    bgms :: Array Sound,
    ses :: Array Sound,
    all :: Array Sound
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


    let bgms = [
            rye,
            yourNatural,
            cleaning
        ]
    let ses = [
            forestSound,
            switchSound,
            pickSound,
            putSound,
            stepSound,
            warpSound
        ]
    pure $ {
        rye,
        yourNatural,
        cleaning,
        forestSound,
        switchSound,
        pickSound,
        putSound,
        stepSound,
        warpSound,

        bgms,
        ses,
        all: bgms <> ses
    }

  where
    load url loop = loadSound url url scene defaultCreateSoundOptions { autoplay = false, loop = loop }

setMute :: forall eff. Boolean -> Sounds -> Eff (babylon :: BABYLON | eff) Unit
setMute mute sounds = for_ sounds.all (setVolume (if mute then 0.0 else 1.0))

setBGMVolume :: forall eff. Number -> Sounds -> Eff (babylon :: BABYLON | eff) Unit
setBGMVolume volume sounds = for_ sounds.bgms (setVolume volume)

stopAllSounds :: forall eff. Sounds -> Eff (babylon :: BABYLON | eff) Unit
stopAllSounds sounds = for_ sounds.all stop
