module Server.Bender (
        generateName,
        generateHeadline,
        generateDescription
) where

import Prelude
import Server.Types

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core as DAC
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple (Tuple(..))
import Effect.Console as EC
import Effect.Random as ER
import Run as R
import Run.Reader as RR
import Server.Response as SRR

generateName :: ServerEffect String
generateName = do
        size <- R.liftEffect $ ER.randomInt 10 30
        generate Name size

generateDescription :: ServerEffect String
generateDescription = do
        size <- R.liftEffect $ ER.randomInt 120 1000
        generate Description size

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
                                url = configuration.benderURL <> "generate?what=" <> show action <> "&max-chars=" <> show size,
                                method = Left GET,
                                responseFormat = RF.json
                        }
                case response.body of
                        Right payload ->
                                if response.status == StatusCode 200 then
                                        DM.maybe (SRR.throwInternalError "could not get text") pure $ DAC.toString payload
                                else
                                        SRR.throwInternalError response.statusText
                        Left left -> SRR.throwInternalError $ RF.printResponseFormatError left
         else do
                number <- R.liftEffect $ ER.randomInt 1 1000000
                pure $ show action <> "-test-" <> show number