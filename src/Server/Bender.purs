module Server.Bender (
        generateName,
        generateHeadline,
        generateDescription
) where

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.FormURLEncoded as DF
import Data.HTTP.Method (Method(..))
import Effect.Console as EC
import Data.Either(Either(..))
import Data.Tuple(Tuple(..))
import Data.Maybe(Maybe(..))
import Run.Reader as RR
import Effect.Random as ER
import Prelude
import Data.String as DS
import Run as R
import Data.String (Pattern(..))
import Server.Types
import Server.Response as SRR

generateName :: ServerEffect String
generateName = pure ""

generateDescription :: ServerEffect String
generateDescription = pure ""

generateHeadline :: ServerEffect String
generateHeadline = do
        let maxSize = 100

        size <- R.liftEffect $ ER.randomInt 1 maxSize
        headline <- generate Description size

        case DS.lastIndexOf (Pattern ".") headline of
                Nothing -> pure headline
                Just index -> do
                        let cutHeadline = DS.take index headline
                        chance <- R.liftEffect $ ER.randomInt 1 100
                        if DS.length cutHeadline <= maxSize - 3 && chance <= 30 then
                                pure $ cutHeadline <> "..."
                         else
                                pure headline

generate :: BenderAction -> Int -> ServerEffect String
generate action size = do
        { configuration : Configuration configuration } <- RR.ask

        if configuration.useBender then do
                response <- R.liftAff <<< A.request $ A.defaultRequest {
                                url = configuration.benderURL <> "generate?what=" <> show action <> "&max-chars" <> show size,
                                method = Left GET,
                                responseFormat = RF.string
                        }
                case response.body of
                        Right payload ->
                                if response.status == StatusCode 200 then
                                        pure payload
                                else
                                        SRR.throwInternalError response.statusText
                        Left left -> SRR.throwInternalError $ RF.printResponseFormatError left
         else do
                number <- R.liftEffect $ ER.randomInt 1 1000000
                pure $ show action <> show "-test-" <> show number