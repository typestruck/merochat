module Server.Wheel where

import Prelude
import Shared.Im.Types

import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))

-- | The karma from a chat is calculated from each "turn" in a conversation:
-- | a turn is all messages from sender (the person who started the chat), recipient replies until the next sender message
-- | For each turn, the karma earned is equal to
-- |    constant * interest_bonus + constant * fast_reply_bonus + sender_bonus
-- | * Constant decays the newer an account is and the older a chat is
-- | * Interest bonus is the ratio of how many characters one user typed to the other
-- | * Fast reply tries to assert how "instantaneous" a given conversation is by making some assumptions about how many characters should have been typed in the time it took for a reply to be sent
-- | * Sender bonus is equal to a third of karma earned
karmaFrom ∷ Turn → Tuple Int Int
karmaFrom { senderStats, recipientStats, chatAge } =
      let
            grossSender = karma senderStats
      in
            Tuple (DI.floor $ grossSender + grossSender * 0.3) (DI.floor $ karma recipientStats)
      where
      karma { interest, characters, replyDelay, accountAge } =
            let
                  constant'
                        | chatAge > days = 0.0
                        | otherwise = constant * (min (accountAge / days) 1.0) * ((days - chatAge) / days)
                  interestBonus = min (DM.fromMaybe 0.0 interest * 0.10) 1.0
                  fastReplyBonus = case replyDelay of
                        Just rd → min (characters / (charactersMinute * rd)) 1.0
                        Nothing → 0.0
            in
                  constant' * interestBonus + constant' * fastReplyBonus

      constant = 50.0
      days = 2.5
      charactersMinute = 200.0
