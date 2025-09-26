module Server.Posts.Database where

import Droplet.Language
import Prelude hiding (join, not)

import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.Fields (_date, _id, _recipient, _sender, l, s, u)
import Server.Database.Histories (histories)
import Server.Database.Messages (_content)
import Server.Database.Posts (_expires, _poster, posts)
import Server.Database.Types (Checked(..))
import Server.Database.Users (_postsVisibility, _temporary, users)
import Server.Effect (ServerEffect)
import Shared.Post (Post, PostPayload)
import Shared.User (ProfileVisibility(..))
import Type.Proxy (Proxy(..))

presentPosts ∷ Int → Int → ServerEffect (Array Post)
presentPosts loggedUserId userId = SD.query $
      select ((p ... _id # as _id) /\ _content /\ _date /\ _expires)
            # from postsSource  #
            wher (postsFilter loggedUserId userId)
            # orderBy (_date # desc)
            # limit (Proxy ∷ _ 8)

postsSource = join (posts # as p) (users # as u) # on (p ... _poster .=. u ... _id)

postsFilter :: Int -> Int -> _
postsFilter loggedUserId userId =
            ( u ... _id .=. userId .&&.
                    ( _postsVisibility .=. Everyone
                            .||. _postsVisibility
                            .=. NoTemporaryUsers
                            .&&. not (exists $ select (1 # as l) # from (users # as s) # wher (s ... _id .=. loggedUserId .&&. _temporary .=. Checked true))
                            .||. _postsVisibility
                            .=. Contacts
                            .&&. (exists $ select (1 # as l) # from (histories # as s) # wher (_sender .=. loggedUserId .&&. _recipient .=. userId .||. _recipient .=. loggedUserId .&&. _sender .=. userId))
                    )
            )

p ∷ Proxy "p"
p = Proxy

savePost ∷ Int → String → ServerEffect Unit
savePost loggedUserId content = SD.execute $ insert # into posts (_content /\ _poster) # values (content /\ loggedUserId)