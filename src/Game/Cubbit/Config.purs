module Game.Cubbit.Config (Config(Config), readConfig, writeConfig) where

import Prelude (bind, pure, Unit)
import Control.Monad.Eff (Eff)
import Data.Generic (class Generic)
import Data.Maybe (fromMaybe)
import DOM (DOM)
import DOM.WebStorage (STORAGE, getItem, setItem, getLocalStorage)

newtype Config = Config {
    mute :: Boolean,
    bgmVolume :: Int,
    seVolume :: Int,
    shadow :: Boolean,
    shadowArea :: Int,
    vertexColor :: Boolean,
    chunkArea :: Int
}

defaultConfig :: Config
defaultConfig = Config {
    mute: false,
    bgmVolume: 3,
    seVolume: 3,
    shadow: true,
    shadowArea: 3,
    vertexColor: true,
    chunkArea: 3
}

derive instance genericConfig :: Generic Config

data ConfigKey a = ConfigKey

derive instance genericConfigKey :: Generic (ConfigKey a)

configKey :: ConfigKey Config
configKey = ConfigKey

readConfig :: forall eff. Eff (storage :: STORAGE, dom :: DOM | eff) Config
readConfig = do
  localStorage <- getLocalStorage
  result <- getItem localStorage configKey
  pure (fromMaybe defaultConfig result)

writeConfig :: forall eff. Config -> Eff (storage :: STORAGE, dom :: DOM | eff) Unit
writeConfig config = do
  localStorage <- getLocalStorage
  setItem localStorage configKey config