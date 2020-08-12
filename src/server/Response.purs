-- | This module wraps HTTPure so it works with our effects. Combinators for responses are provided alongside file serving for development enviroments.
module Server.Response(
      html,
      json,
      json',
      serveDevelopmentFile,
      requestError,
      throwInternalError,
      throwBadRequest,
      redirect,
      serveTemplate,
      headerContentType
) where

import Prelude
import Server.Types
import Shared.Types

import Data.Argonaut.Core as DAC
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode as DAE
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Parser as DAP
import Data.Array as DA
import Data.Either as DET
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.Read as DSR
import Effect (Effect)
import Effect.Console as EC
import HTTPure (Response)
import HTTPure as H
import HTTPure.Body (class Body)
import HTTPure.Headers (Headers)
import Node.FS.Aff as NFA
import Node.Path as NP
import Partial.Unsafe as PU
import Run (Run, AFF, EFFECT)
import Run as R
import Run.Except as RE
import Server.NotFound.Template as SNT
import Shared.JSON as SJ
import Shared.Router as SR
import Shared.Unsafe as SU

html :: forall e. String -> Run (aff :: AFF | e) Response
html contents = ok' (headerContentType $ show HTML) contents

--REFACTOR: user Shared.Json
-- | Parses the request body as JSON, feeds it to a handler and serializes the return as a JSON response
json :: forall a b c d. Generic a b => EncodeRep b => Generic c d => DecodeRep d => String -> (c -> ServerEffect a) -> ResponseEffect
json body handler = DET.either (RE.throw <<< InternalError <<< { reason : _ }) runHandler $ DAP.jsonParser body
      where runHandler arg = do
                  response <- handler $ PU.unsafePartial (DET.fromRight $ DADGR.genericDecodeJson arg)
                  json' response

json' :: forall response r. Generic response r => EncodeRep r => response -> ResponseEffect
json' = ok' (headerContentType $ show JSON) <<< SJ.toJSON

serveDevelopmentFile :: Array String -> ResponseEffect
serveDevelopmentFile path = do
      contents <- R.liftAff $ NFA.readFile fullPath
      ok' (contentTypeFromExtension fullPath) contents
      where clientBaseFolder = "src/client/"
            distBaseFolder = "dist/"
            fullPath = case path of
                ["client", "media", file] -> clientBaseFolder <> "media/" <> file
                ["client", "media", "upload", file] -> clientBaseFolder <>  "media/upload/" <> file
                --js files are expected to be named like module.bundle.js
                -- they are served from parcel output
                ["client", "javascript", file] -> distBaseFolder <> file
                ["client", folder, file] -> clientBaseFolder <> folder <> "/" <> file
                _ -> distBaseFolder <> DS.joinWith "/" path

contentTypeFromExtension :: String -> Headers
contentTypeFromExtension = headerContentType <<< show <<< read <<< NP.extname
      where read :: String -> ContentType
            read = SU.fromJust <<< DSR.read

headerContentType :: String -> Headers
headerContentType = H.header "Content-Type"

requestError :: ResponseError -> Run (aff :: AFF, effect :: EFFECT) Response
requestError ohno = do
      R.liftEffect <<< EC.log $ "server error " <> show ohno
      case ohno of
            BadRequest { reason } -> jsonError H.badRequest' reason
            InternalError { reason } -> jsonError H.internalServerError' reason
            AnonymousRequired -> redirect IM
            LoginRequired { next, isPost } ->
                  if isPost then
                        jsonError (const <<< H.forbidden') next
                   else
                        redirect $ Login { next: Just next }
            NotFound { reason, isPost } ->
                  if isPost then
                        jsonError (const <<< H.notFound') reason
                   else do
                        contents <- R.liftEffect SNT.template
                        html contents
      where jsonError handler = R.liftAff <<< handler (headerContentType $ show JSON) <<< DAC.stringify <<< DAE.encodeJson

throwInternalError :: forall whatever.  String -> ServerEffect whatever
throwInternalError reason = RE.throw $ InternalError { reason }

throwBadRequest :: forall whatever. String -> ServerEffect whatever
throwBadRequest reason = RE.throw $ BadRequest { reason }

redirect :: forall e. Route -> Run (aff :: AFF | e) Response
redirect = R.liftAff <<< flip H.temporaryRedirect' "" <<< H.header "Location" <<< SR.fromRoute

serveTemplate :: Effect String -> ResponseEffect
serveTemplate template = do
      contents <- R.liftEffect template
      html contents

ok' :: forall response e. Body response => Headers -> response -> Run (aff :: AFF | e) Response
ok' headers = R.liftAff <<< H.ok' headers