module Server.Wheel where

import Prelude
import Shared.IM.Types

import Data.Int53 as DI
import Data.String as DS
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Shared.Types (PrimaryKey(..))

foreign import sendWheelMessage_ :: EffectFn1 String Unit

sendMessage :: PrimaryKey -> PrimaryKey -> Turn -> Effect Unit
sendMessage sender recipient turn = EU.runEffectFn1 sendWheelMessage_ $ messageLine sender recipient turn

messageLine :: PrimaryKey -> PrimaryKey -> Turn -> String
messageLine sender recipient (Turn {senderStats, recipientStats, chatAge, replyDelay}) =
        DS.joinWith
         ";" [messageFrom sender senderStats, messageFrom recipient recipientStats, comma [chatAge, replyDelay]]

messageFrom :: PrimaryKey -> Stats -> String
messageFrom (PrimaryKey id) (Stats {characters, interest}) = comma [DI.toNumber id, characters, interest]

comma :: Array Number -> String
comma = DS.joinWith "," <<< map show