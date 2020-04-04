module Shared.Profile.Types where

import Shared.Types

import Data.Generic.Rep (class Generic)

newtype ProfileUser = ProfileUser {

}

data ProfileMessage = ProfileMessage


derive instance genericProfileUser :: Generic ProfileUser _