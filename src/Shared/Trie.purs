--refactor: make a library instead of relying on js
module Shared.Trie where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2)
import Data.Function.Uncurried as DFU
import Shared.Types

foreign import makeTrie_ :: Fn1 (Array { k :: String, s :: String }) Trie
foreign import complete_ :: Fn2 Trie String (Array { k :: String, s :: String })

makeTrie :: Array { k :: String, s :: String } -> Trie
makeTrie = DFU.runFn1 makeTrie_

complete :: Trie -> String -> Array String
complete trie key = _.s <$> DFU.runFn2 complete_ trie key