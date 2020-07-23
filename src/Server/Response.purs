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
      serveTemplate
) where

import Prelude
import Server.Types
import Shared.Types

import Data.Argonaut.Core as DAC
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode as DAE
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Argonaut.Parser as DAP
import Data.Either as DE
import Data.Either as DET
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String.Read as DSR
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console as EC
import HTTPure (Headers, Response, ResponseM, Path)
import HTTPure as H
import HTTPure.Body (class Body)
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

html :: String -> ResponseEffect
html contents = ok' (headerContentType $ show HTML) contents

-- | Parses the request body as JSON, feeds it to a handler and serializes the return as a JSON response
json :: forall a b c d. Generic a b => EncodeRep b => Generic c d => DecodeRep d => String -> (c -> ServerEffect a) -> ResponseEffect
json body handler = DET.either (RE.throw <<< InternalError <<< { reason : _ }) runHandler $ DAP.jsonParser body
      where   runHandler arg = do
                  response <- handler $ PU.unsafePartial (DET.fromRight $ DADGR.genericDecodeJson arg)
                  json' response

json' :: forall response r. Generic response r => EncodeRep r => response -> ResponseEffect
json' = ok' (headerContentType $ show JSON) <<< SJ.toJSON

serveDevelopmentFile :: Array String -> ResponseEffect
serveDevelopmentFile path = do
      contents <- R.liftAff <<< NFA.readFile $ "src/Client/" <> folder <> "/" <> fileName
      ok' (contentTypeFromExtension fileName) contents
      where Tuple folder fileName = case path of
                ["client", "media", file] -> Tuple "media" file
                ["client", "media", "upload", file] -> Tuple "media/upload" file
                ["client", folder, file] -> Tuple folder file
                _ -> Tuple "invalidFolder" "invalidFile"

contentTypeFromExtension :: String -> Headers
contentTypeFromExtension = headerContentType <<< show <<< read <<< NP.extname
      where read :: String -> ContentType
            read = SU.fromJust "contentTypeFromExtension" <<< DSR.read

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
                        ok' (headerContentType $ show HTML) contents
      where jsonError handler = R.liftAff <<< handler (headerContentType $ show JSON) <<< DAC.stringify <<< DAE.encodeJson

throwInternalError :: forall whatever. String -> ServerEffect whatever
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