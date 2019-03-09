module Shared.Routing where

import Routing.Parser as P
import Data.Foldable as F
import Prelude
import Shared.Types
import Routing.Match as M
import Data.Maybe(Maybe(..))
import Routing.Match (Match)
import Control.Alt ((<|>))

-- finish here

routes :: Match Route
routes = F.oneOf [
	Landing <$ M.lit "",
	Landing <$ M.lit "/",
	Register <$ M.lit "register",
	Login <$> (M.lit "login" *> (Just <$> M.param "next" <|> pure Nothing))
]