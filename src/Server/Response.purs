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
import Data.Argonaut.Encode as DAE
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Argonaut.Parser as DAP
import Data.Either as DE
import Data.Generic.Rep (class Generic)
import Shared.Unsafe as SU
import Shared.JSON as SJ
import Data.Maybe as DM
import Data.String.Read as DSR
import Effect.Console as EC
import HTTPure (Headers, Response, ResponseM, Path)
import HTTPure as H
import HTTPure.Body (class Body)
import Data.Either as DET
import Node.FS.Aff as NFA
import Node.Path as NP
import Partial.Unsafe as PU
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Effect(Effect)
import Run (Run, AFF, EFFECT)
import Run as R
import Run.Except as RE
import Server.NotFound.Template as SNT

html :: String -> ResponseEffect
html contents = ok' (headerContentType $ show HTML) contents

json :: forall a b c d. Generic a b => EncodeRep b => Generic c d => DecodeRep d => String -> (c -> ServerEffect a) -> ResponseEffect
json body handler = DET.either (RE.throw <<< InternalError <<< { reason : _ }) runHandler $ DAP.jsonParser body
        where   runHandler arg = do
                        response <- handler $ PU.unsafePartial (DET.fromRight $ DADGR.genericDecodeJson arg)
                        json' response

json' :: forall response r. Generic response r => EncodeRep r => response -> ResponseEffect
json' = ok' (headerContentType $ show JSON) <<< SJ.toJSON

serveDevelopmentFile :: String -> String -> ResponseEffect
serveDevelopmentFile folder fileName = do
              contents <- R.liftAff <<< NFA.readFile $ "src/Client/" <> folder <> "/" <> fileName
              ok' (contentTypeFromExtension fileName) contents

contentTypeFromExtension :: String -> Headers
contentTypeFromExtension = headerContentType <<< show <<< read <<< NP.extname
        where   read :: String -> ContentType
                read = SU.unsafeFromJust "contentTypeFromExtension" <<< DSR.read

headerContentType :: String -> Headers
headerContentType = H.header "Content-Type"

requestError :: ResponseError -> Run (aff :: AFF, effect :: EFFECT) Response
requestError ohno = do
        R.liftEffect <<< EC.log $ "internal server error " <> show ohno
        case ohno of
                BadRequest { reason } -> liftedJSONResponse H.badRequest' reason
                InternalError { reason } -> liftedJSONResponse H.internalServerError' reason
                NotFound { reason, isPost } ->
                        if isPost then
                                liftedJSONResponse (const <<< H.notFound') reason
                         else do
                                contents <- R.liftEffect SNT.template
                                R.liftAff $ H.ok' (headerContentType $ show HTML) contents

        where liftedJSONResponse handler = R.liftAff <<< handler (headerContentType $ show JSON) <<< DAC.stringify <<< DAE.encodeJson

throwInternalError :: forall whatever. String -> ServerEffect whatever
throwInternalError reason = RE.throw $ InternalError { reason }

throwBadRequest :: forall whatever. String -> ServerEffect whatever
throwBadRequest reason = RE.throw $ BadRequest { reason }

redirect :: String -> ResponseEffect
redirect = R.liftAff <<< flip H.temporaryRedirect' "" <<< H.header "Location"

serveTemplate :: Effect String -> ResponseEffect
serveTemplate template = do
        contents <- R.liftEffect template
        html contents

ok' :: forall response. Body response => Headers -> response -> ResponseEffect
ok' headers = R.liftAff <<< H.ok' headers