module Shared.Html where

import Prelude

import Data.Either (Either(..))
import Payload.Client.DecodeResponse (class DecodeResponse)
import Payload.Client.DecodeResponse as PCD
import Payload.Client.Fetch as PCF
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes as PR
import Payload.Server.Response (class EncodeResponse)

newtype Html = Html String

instance EncodeResponse Html where
      encodeResponse (PR.Response { status, headers, body: Html contents }) = pure $ PR.Response
            { headers: PH.setIfNotDefined "content-type" html headers
            , body: PR.StringBody contents
            , status
            }

instance  DecodeResponse Html where
  decodeResponse resp = do
      text <- PCF.text resp.raw
      case text of
            Right t -> pure <<< Right $ Html t
            Left l -> pure <<< Left <<< PCD.unknown $ show l