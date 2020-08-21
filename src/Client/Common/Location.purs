module Client.Common.Location where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Routing.Duplex.Parser (RouteError)

import Web.HTML as WH
import Web.HTML.Location as WHL
import Web.HTML.Window as WHW

--setLocation :: Route -> Effect Unit
setLocation route = do
        window <- WH.window
        location <- WHW.location window
        --WHL.setHref (SR.fromRoute route) location
        WHL.setHref route location

--search :: Effect (Either RouteError Route)
search = do
        window <- WH.window
        location <- WHW.location window
    --    SR.toRoute <$> WHL.search location
        WHL.search location

--path :: Effect (Either RouteError Route)
path = do
        window <- WH.window
        location <- WHW.location window
        search' <-WHL.search location
        pathname <- WHL.pathname location
      --  pure <<< SR.toRoute $ pathname <> search'
        pure $ pathname <> search'
