module LZString where

foreign import compress :: String -> String

foreign import decompress :: String -> String