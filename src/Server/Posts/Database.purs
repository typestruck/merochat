module Server.Posts.Database where

import Droplet.Language
import Prelude

import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.Fields (_date, _id)
import Server.Database.Messages (_content)
import Server.Database.Posts (_expires, _poster, posts)
import Server.Effect (ServerEffect)
import Shared.Post (Post, PostPayload)
import Type.Proxy (Proxy(..))

--need to check post privacy settings too
presentPosts ∷ Int → Int → ServerEffect (Array Post)
presentPosts loggedUserId userId = SD.query $ select (_id /\ _content /\ _date /\ _expires) # from posts # wher (_poster .=. userId) # orderBy (_date # desc) # limit (Proxy ∷ _ 8)

savePost ∷ Int → PostPayload → ServerEffect Unit
savePost loggedUserId payload = SD.execute $ insert # into posts (_content /\ _poster) # values (payload.content /\ loggedUserId)