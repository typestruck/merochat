module Client.IM.WebSocket where

import Prelude

import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Shared.JSON as SJ
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW

sendPayload :: forall payload p. Generic payload p => EncodeRep p => WebSocket -> payload -> Effect Unit
sendPayload ws = WSW.sendString ws <<< SJ.toJSON