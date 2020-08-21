module Server.InternalError.Handler where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)

import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Server.InternalError.Template as SIT

internalError :: String -> Effect (Response String)
internalError message = do
      contents <- SIT.template message
      pure <<< PSR.setHeaders (PH.fromFoldable [Tuple "content-type" html]) $ PSR.internalError contents