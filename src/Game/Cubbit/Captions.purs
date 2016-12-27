module Game.Cubbit.Captions where

import Data.Eq (class Eq)
import Data.Generic (class Generic)
import Data.Show (class Show)

data Language = Ja | En

derive instance genericLanguage :: Generic Language

instance showLanguage :: Show Language where
    show Ja = "ja"
    show En = "en"

derive instance eqLanguage :: Eq Language

type Caption = {
    ja :: String,
    en :: String
}

type Captions = {
    modeSelection :: Caption,
    singleplayerOfflineMode :: Caption,
    multiplayerOnlineMode :: Caption
}

getCaption :: Language -> (Captions -> Caption) -> String
getCaption lang f = (case lang of
    En -> _.en
    Ja -> _.ja) (f captions)

captions :: Captions
captions = {
    modeSelection: {
        ja: "モード選択",
        en: "Mode Selection"
    },
    singleplayerOfflineMode: {
        ja: "シングルプレイヤー・オフラインモード",
        en: "Siingle Player Offline Mode"
    },
    multiplayerOnlineMode: {
        ja: "マルチプレイヤー・オンラインモード",
        en: "Multi-player Online Mode"
    }
}
