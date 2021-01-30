module Server.ThreeK (
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
import Effect.Uncurried (EffectFn3)
import Effect.Uncurried as EU
import Run as R
import Run.Reader as RR
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, nameMaxCharacters)

foreign import generate_ :: EffectFn3 Boolean Int Int String

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

generate :: ThreeKAction -> Int -> ServerEffect String
generate action size = do
        { configuration: { randomizeProfiles } } <- RR.ask
        R.liftEffect $ EU.runEffectFn3 generate_ randomizeProfiles (DE.fromEnum action) size