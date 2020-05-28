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
import Data.Enum as DE
import Run.Reader as RR
import Effect.Uncurried as EU
import Effect.Uncurried(EffectFn2)
import Server.Response as SRR

foreign import generate_ :: EffectFn2 Int Int String

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
generate action size = R.liftEffect $ EU.runEffectFn2 generate_ (DE.fromEnum action) size