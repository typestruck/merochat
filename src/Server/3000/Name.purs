module Server.ThreeK.Name where

import Prelude

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap as HM
import Data.List (List(..))
import Data.List as DL
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect (Effect)
import Effect.Random as ER
import Server.ThreeK.Data (GrammaticalClass(..), grammaticalClasses)
import Shared.Unsafe as SU

-- | A "simple name" is a string containing <adjective> [, <adjective>] <noun>
simpleName ∷ Int → Effect String
simpleName remainingChars = do
      names ← makeName remainingChars classesChances
      pure $ case names of
            [ n, n1, n2 ] → n <> ", " <> n1 <> " " <> n2
            more → DS.joinWith " " more
      where
      classesChances =
            [ Tuple (Right Adjective) 100
            , Tuple (Right Adjective) 40
            , Tuple (Right Noun) 100
            ]

-- | A "complex name" is a string containing [<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>]
complexName ∷ Int → Effect String
complexName remainingChars = do
      whoChance ← shouldHappen 70
      let
            classesChances =
                  [ Tuple (Right Adjective) 20
                  , Tuple (Right Noun) 100
                  , Tuple (Left $ if whoChance then "who" else "that") 100
                  , Tuple (Right Adverb) 20
                  , Tuple (Right Verb) 100
                  , Tuple (Right Adjective) 10
                  , Tuple (Right PluralNoun) 30
                  , Tuple (Right Other) 5
                  ]
      names ← makeName remainingChars classesChances
      pure $ DS.joinWith " " names

makeName ∷ Int → Array (Tuple (Either String GrammaticalClass) Int) → Effect (Array String)
makeName remainingChars classesChances = do
      classes ← filterByChance classesChances
      make remainingChars $ DL.fromFoldable classes
      where
      make remaining = case _ of
            Cons gc rest → do
                  let
                        add word size = do
                              more ← make (remaining - size) rest
                              pure $ word : more
                  case gc of
                        Right c → do
                              word ← getWord c remaining
                              DM.maybe (make remaining rest) (\w → add w $ DS.length w) word
                        Left word → do
                              let size = DS.length word

                              if remaining - 1 >= size then
                                    add word size
                              else
                                    make remaining rest
            Nil → pure []

-- | Pick a grammatical class random word up to the given size
getWord ∷ GrammaticalClass → Int → Effect (Maybe String)
getWord gClass remainingChars = do
      let
            sizeWords = SU.fromJust $ HM.lookup gClass grammaticalClasses
            keys = DA.filter (_ <= remainingChars - 1) $ HM.keys sizeWords
            len = DA.length keys

      if len > 0 then do
            keyIndex ← ER.randomInt 0 (len - 1)
            let words = SU.fromJust do
                  size <- keys !! keyIndex
                  HM.lookup size sizeWords
            wordIndex ← ER.randomInt 0 $ DA.length words
            pure $ words !! wordIndex
      else
            pure Nothing

-- | Given a list, returns values according to their chance of occurring
filterByChance ∷ Array (Tuple (Either String GrammaticalClass) Int) → Effect (Array (Either String GrammaticalClass))
filterByChance classes = map DT.fst <$> DA.filterA (shouldHappen <<< DT.snd) classes

-- | Chance of an event occurring out of 100 times
shouldHappen ∷ Int → Effect Boolean
shouldHappen chance = (_ <= chance) <$> ER.randomInt 1 100
