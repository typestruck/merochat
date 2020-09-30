module Server.Bender (
        generateName,
        generateHeadline,
        generateDescription
) where

import Prelude
import Server.Types

import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Effect.Random as ER
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Run as R
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, nameMaxCharacters)

foreign import generate_ :: EffectFn2 Int Int String

generateName :: ServerEffect String
generateName = do
        size <- R.liftEffect $ ER.randomInt 10 nameMaxCharacters
        generate Name size

generateDescription :: ServerEffect String
generateDescription = do
        size <- R.liftEffect $ ER.randomInt 120 descriptionMaxCharacters
        generate Description size

generateHeadline :: ServerEffect String
generateHeadline = do
        let maxSize = headlineMaxCharacters

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