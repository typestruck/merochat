module Client.Common.Storage where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Web.HTML as WH
import Web.HTML.Window as WHW
import Web.Storage.Storage as WSS
import Data.Maybe as DM

tokenKey :: String
tokenKey = "token"

setItem :: String -> String -> Effect Unit
setItem key itemValue = do
        window <- WH.window
        localStorage <- WHW.localStorage window
        WSS.setItem key itemValue localStorage

getItem :: String -> Effect String
getItem key = do
        window <- WH.window
        localStorage <- WHW.localStorage window
        DM.fromMaybe "" <$> WSS.getItem key localStorage

--used to auth web socket operations
getToken :: Effect String
getToken = getItem tokenKey

removeItem :: String -> Effect Unit
removeItem key = do
        window <- WH.window
        localStorage <- WHW.localStorage window
        WSS.removeItem key localStorage