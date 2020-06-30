module Client.IM.WebSocket where

import Shared.JSON as SJ
import Web.Socket.WebSocket as WSW
import Client.Common.Types
import Prelude

sendPayload ws = WSW.sendString ws <<< SJ.toJSON