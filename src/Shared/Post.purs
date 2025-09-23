module Shared.Post where

import Prelude

import Data.Maybe (Maybe)
import Shared.DateTime (DateTimeWrapper(..))

type Post = {
    id :: Int,
    date :: DateTimeWrapper,
    content :: String,
    expires :: Maybe DateTimeWrapper
}

type PostPayload = {
    content :: String
}