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

type L = Language -> String

l :: String -> String -> L
l en ja = case _ of
    Ja -> ja
    En -> en

modeSelection :: L
modeSelection = l "Mode Selection" "モード選択"

singleplayerOfflineMode :: L
singleplayerOfflineMode = l "Siingle Player Offline Mode" "シングルプレイヤー・オフラインモード"

multiplayerOnlineMode :: L
multiplayerOnlineMode = l "Multi-player Online Mode" "マルチプレイヤー・オンラインモード"

clickToStart :: L
clickToStart = l "Click to Start" "マウスクリックでスタートします"

on :: L
on = l "On" "オン"

off :: L
off = l "Off" "オフ"

graphics :: L
graphics = l "Graphics" "グラフィックス"

sounds :: L
sounds = l "Sound" "サウンド"

terrain :: L
terrain = l "Terrain" "地形表示"

language :: L
language = l "Language" "言語"

mute :: L
mute = l "Mute" "消音"

bgmVolume :: L
bgmVolume = l "BGM Volume" "ＢＧＭ音量"

seVolume :: L
seVolume = l "SE Volume" "ＳＥ音量"

chunkArea :: L
chunkArea = l "Chunk Display Area" "チャンク表示距離"

shadow :: L
shadow = l "Shadow" "影"

shadowArea :: L
shadowArea = l "Shadow Area" "影の範囲"

vertexColor :: L
vertexColor = l "Vertex Color" "頂点色"