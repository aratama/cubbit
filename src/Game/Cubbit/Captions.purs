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

type L = Language -> String

clickToStart :: L
clickToStart En = "Click to Start"
clickToStart Ja = "マウスクリックでスタートします"

on :: L
on En = "On"
on Ja = "オン"

off :: L
off En = "Off"
off Ja = "オフ"

graphics :: L
graphics Ja = "グラフィックス"
graphics En = "Graphics"

sounds :: L
sounds Ja = "サウンド"
sounds En = "Sound"

terrain :: L
terrain Ja = "地形表示"
terrain En = "Terrain"

language :: L
language Ja = "言語"
language En = "Language"

mute :: L
mute Ja = "消音"
mute En = "Mute"

bgmVolume :: L
bgmVolume Ja = "ＢＧＭ音量"
bgmVolume En = "BGM Volume"

seVolume :: L
seVolume Ja = "ＳＥ音量"
seVolume En = "SE Volume"

chunkArea :: L
chunkArea Ja = "チャンク表示距離"
chunkArea En = "Chunk Display Area"

shadow :: L
shadow Ja = "影"
shadow En = "Shadow"

shadowArea :: L
shadowArea Ja = "影の範囲"
shadowArea En = "Shadow Area"

vertexColor :: L
vertexColor Ja = "頂点色"
vertexColor En = "Vertex Color"