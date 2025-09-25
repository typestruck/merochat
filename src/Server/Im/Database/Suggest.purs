module Server.Im.Database.Suggest where

import Debug
import Droplet.Language
import Prelude hiding (not, join)
import Server.Database.Badges
import Server.Database.BadgesUsers
import Server.Database.Fields
import Server.Database.Suggestions
import Shared.Privilege

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as DAN
import Data.BigInt as DB
import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Time.Duration (Days(..), Minutes(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.Blocks (_blocked, _blocker, blocks)
import Server.Database.Countries (countries)
import Server.Database.Functions (date_part_age, datetime_part_age, insert_history, utc_now)
import Server.Database.Histories (_first_message_date, _recipient_deleted_to, _sender_deleted_to, histories)
import Server.Database.KarmaHistories (_amount, _target, karma_histories)
import Server.Database.KarmaLeaderboard (_current_karma, _karma, _karmaPosition, _position, _ranker, karma_leaderboard)
import Server.Database.Languages (_languages, languages)
import Server.Database.LanguagesUsers (_language, _speaker, languages_users)
import Server.Database.LastSeen (_who, last_seen)
import Server.Database.Messages (_content, _status, messages, _edited)
import Server.Database.Posts (_poster, _totalPosts, _unseenPosts)
import Server.Database.Privileges (_feature, _privileges, _quantity, privileges)
import Server.Database.Posts (posts)
import Server.Database.Reports (_comment, _reason, _reported, _reporter, reports)
import Server.Database.Tags (_tags, tags)
import Server.Database.TagsUsers (_creator, _tag, tags_users)
import Server.Database.Types (Checked(..))
import Server.Database.Users (_avatar, _birthday, _completedTutorial, _country, _description, _email, _gender, _headline, _isContact, _joined, _messageTimestamps, _onlineStatus, _password, _postsVisibility, _readReceipts, _temporary, _typingStatus, _visibility, _visibility_last_updated, users)
import Server.Effect (BaseEffect, ServerEffect)
import Server.Im.Database.Flat (FlatContactHistoryMessage, FlatUser, FlatContact)
import Server.Im.Database.Present (completeness, userFields, usersSource)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as ST
import Shared.Im.Types (HistoryMessage, MessageStatus(..), Report, SuggestionsFrom(..))
import Shared.Options.Page (contactsPerPage, initialMessagesPerPage, messagesPerPage)
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))
import Type.Proxy (Proxy(..))

suggest ∷ Int → Int → SuggestionsFrom → ServerEffect (Array FlatUser)
suggest loggedUserId skip =
      case _ of
            OnlineOnly → SD.query $ suggestOnlineQuery loggedUserId skip onlineFilter
            ThisWeek → SD.query $ suggestMainQuery loggedUserId skip thisWeekFilter
            LastTwoWeeks → SD.query $ suggestMainQuery loggedUserId skip lastTwoWeeksFilter
            LastMonth → SD.query $ suggestMainQuery loggedUserId skip lastMonthFilter
            All → SD.query $ suggestAllQuery loggedUserId skip baseFilter

      where
      onlineFilter = baseFilter .&&. (l ... _date) .>=. (ST.unsafeAdjustFromNow $ Minutes (-1.0))
      thisWeekFilter = baseFilter .&&. (l ... _date) .>=. (ST.unsafeAdjustFromNow $ Days (-7.0))
      lastTwoWeeksFilter = baseFilter .&&. (l ... _date) .>=. (ST.unsafeAdjustFromNow $ Days (-14.0))
      lastMonthFilter = baseFilter .&&. (l ... _date) .>=. (ST.unsafeAdjustFromNow $ Days (-30.0))

      baseFilter = u ... _id .<>. loggedUserId .&&. visibilityFilter .&&. blockedFilter
      visibilityFilter =
            _visibility .=. Everyone .&&. _temporary .=. Checked false .||. _visibility .=. NoTemporaryUsers .&&. not (exists $ select (1 # as u) # from users # wher (_id .=. loggedUserId .&&. _temporary .=. Checked true))
      blockedFilter = not (exists $ select (1 # as u) # from blocks # wher (_blocker .=. loggedUserId .&&. _blocked .=. u ... _id .||. _blocker .=. u ... _id .&&. _blocked .=. loggedUserId))

-- top level to avoid monomorphic filter
suggestBaseQuery loggedUserId filter =
      select
            ( userFields
                    /\ _bin
                    /\ completeness
                    /\ (isNotNull _sender # as _isContact)
                    /\ ((select (count _id # as _totalPosts) # from posts  # wher (postsFilter loggedUserId) # orderBy _totalPosts # limit (Proxy ∷ _ 1)) # as _totalPosts)
                    /\ ((select (count _id # as _unseenPosts) # from posts # wher (( postsFilter loggedUserId  .&&._date .>=. DateTimeWrapper (ST.unsafeAdjustFromNow (Days (-1.0))))) # orderBy _unseenPosts # limit (Proxy ∷ _ 1)) # as _unseenPosts)
            )
            # from (leftJoin (join usersSource (suggestions # as s) # on (u ... _id .=. _suggested)) (histories # as h) # on (_sender .=. u ... _id .&&. _recipient .=. (loggedUserId ∷ Int) .||. _sender .=. loggedUserId .&&. _recipient .=. u ... _id))
            # wher filter

postsFilter ∷ Int → _
postsFilter loggedUserId =
      ( _poster .=. u ... _id .&&.
              ( u ... _postsVisibility .=. Everyone .||.
                u ... _postsVisibility .=. NoTemporaryUsers .&&. not (exists $ select (1 # as l) # from (users # as s) # wher (s ... _id .=. loggedUserId .&&. _temporary .=. Checked true)) .||.
                u ... _postsVisibility .=. Contacts .&&. isNotNull (h ... _sender)
              )
      )

suggestMainQuery loggedUserId skip filter =
      suggestBaseQuery loggedUserId filter
            # orderBy ((_sender # desc) /\ _bin /\ (l ... _date # desc))
            # limit (Proxy ∷ Proxy 10)
            # offset skip

suggestAllQuery loggedUserId skip filter = suggestBaseQuery loggedUserId filter # orderBy ((_score # desc) /\ (l ... _date # desc)) # limit (Proxy ∷ Proxy 10) # offset skip

suggestOnlineQuery loggedUserId skip filter =
      suggestBaseQuery loggedUserId filter
            # orderBy ((_sender # desc) /\ _bin /\ (l ... _date # desc))
            # limit (Proxy ∷ Proxy 10)
            # offset skip
