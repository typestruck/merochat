-- | Karma score calculation
module Server.Wheel where

import Prelude
import Shared.Im.Types

import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | The karma from a chat is calculated from each "turn" in a conversation:
-- | a turn is all messages from sender (the person who started the chat), recipient replies until the next sender message
-- | For each turn, the karma earned is equal to
-- |    constant * interest_bonus + constant * new_chat_bonus + constant * fast_reply_bonus + sender_bonus
-- | * Newer accounts have a smaller constant
-- | * Interest bonus is the ratio of how many characters one user typed to the other
-- | * New chat bonus is how fresh a chat is, more karma should be earned on the first day of a chat than that on the 30000th
-- | * Fast reply tries to assert how "instantaneous" a given conversation is by making some assumptions about how many characters should have been typed in the time it took for a reply to be sent
-- | * Sender bonus is equal to half of karma earned
karmaFrom ∷ Turn → Tuple Int Int
karmaFrom { senderStats, recipientStats, chatAge } =
      let
            grossSender = karma senderStats
      in
            Tuple (DI.floor $ grossSender + grossSender * 0.5) (DI.floor $ karma recipientStats)
      where
      karma { interest, characters, replyDelay, accountAge } =
            let
                  constant' = constant / max (days - days * accountAge) 1.0
                  interestBonus = min (interest * 0.10) 1.0
                  newChatBonus = (days - min days chatAge) / days
                  fastReplyBonus = case replyDelay of
                        Just rd → min (characters / (charactersMinute * rd)) 1.0
                        Nothing → 0.0
            in
                  constant' * interestBonus + constant' * newChatBonus + constant' * fastReplyBonus

      constant = 50.0
      days = 7.0
      charactersMinute = 200.0
