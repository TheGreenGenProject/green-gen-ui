module Data.Hash exposing (..)

import MD5
type Hash = Hash String

md5: String -> Hash
md5 = MD5.hex >> Hash

toString: Hash -> String
toString (Hash str) = str




