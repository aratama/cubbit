module LZString where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Prelude ((<<<))

foreign import compress :: String -> String

foreign import _decompress :: String -> Nullable String

decompress :: String -> Maybe String
decompress = toMaybe <<< _decompress

foreign import compressToUTF16 :: String -> String

foreign import _decompressFromUTF16 :: String -> Nullable String

decompressFromUTF16 :: String -> Maybe String
decompressFromUTF16 = toMaybe <<< _decompressFromUTF16