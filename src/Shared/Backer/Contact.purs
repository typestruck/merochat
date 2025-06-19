module Shared.Backer.Contact (backer, backerId) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.Availability (Availability(..))
import Shared.DateTime (DateTimeWrapper(..), epoch)
import Shared.Im.Contact as SC
import Shared.Im.Types (Contact, MessageStatus(..), User)
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.User (ProfileVisibility(..))

backerId ∷ Int
backerId = 0

backer ∷ Contact
backer = (SC.defaultContact backerId user)
      { history =
              [ { sender: backerId
                , recipient: 0
                , date: DateTimeWrapper $ EU.unsafePerformEffect EN.nowDateTime
                , edited: false
                , content: "MeroChat depends on you to keep running"
                , status: Read
                , id: 0
                }
              ]
      }

user ∷ User
user =
      { id: backerId
      , name: "Donate to MeroChat!"
      , availability: Online
      , backer: false
      , joined: DateTimeWrapper epoch
      , readReceipts: true
      , messageTimestamps: true
      , typingStatus: true
      , onlineStatus: true
      , completedTutorial: true
      , avatar: Just $ SP.resourcePath (Left BackerAvatar) Png
      , headline: ""
      , description: ""
      , tags: []
      , karma: 0
      , karmaPosition: 0
      , gender: Nothing
      , privileges: []
      , bin: 1
      , badges: []
      , temporary: false
      , country: Nothing
      , languages: []
      , profileVisibility: Everyone
      , age: Just 0
      }