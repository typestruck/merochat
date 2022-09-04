module Shared.Html where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as DS
import Data.String.Read (class Read)
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes as PR
import Payload.Server.Response (class EncodeResponse)

newtype Html = Html String

derive instance Newtype Html _

instance EncodeResponse Html where
      encodeResponse (PR.Response { status, headers, body: Html contents }) = pure $ PR.Response
            { headers: PH.setIfNotDefined "content-type" html headers
            , body: PR.StringBody contents
            , status
            }