module Shared.Backer.Contact (backerContact, backerId, backerUser) where

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
import Shared.User (ProfileTab(..), ProfileVisibility(..))

backerId ∷ Int
backerId = 0

backerContact ∷ Int → Contact
backerContact userId = (SC.defaultContact backerId backerUser)
      { history =
              [ { sender: backerId
                , recipient: userId
                , date: DateTimeWrapper $ EU.unsafePerformEffect EN.nowDateTime
                , edited: false
                , reaction: Nothing
                , content: "MeroChat depends on you to keep running"
                , status: Read
                , id: 0
                }
              ]
      }

backerUser ∷ User
backerUser =
      { id: backerId
      , name: "Support MeroChat!"
      , availability: Online
      , backer: false
      , joined: DateTimeWrapper epoch
      , readReceipts: true
      , posts: []
      , favorite : false
      , asks: []
      , asksVisibility: Everyone
      , totalAsks:0
      , ownBackground: false
      , chatBackground: Nothing
      , showing: ShowInfo
      , messageTimestamps: true
      , postsVisibility: Everyone
      , unseenPosts: 0
      , isContact: false
      , totalPosts: 0
      , typingStatus: true
      , onlineStatus: true
      , completedFields: []
      , completedTutorial: true
      , avatar: Just $ SP.resourcePath (Left BackerAvatar) Png
      , headline: "MeroChat depends on you to keep running"
      , description:
              """Hey you!
Yes, you!
Donate today if you want to save the only good place to chat online
❤️"""
      , tags: [ "merochat", "you", "support" ]
      , karma: 10000000
      , karmaPosition: 234
      , gender: Nothing
      , privileges: []
      , bin: 2
      , badges: []
      , temporary: false
      , country: Nothing
      , languages: []
      , profileVisibility: Everyone
      , age: Just 39
      }