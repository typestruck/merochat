module Server.Posts.Database where

import Droplet.Language
import Prelude hiding (join, not)

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Server.Database as SD
import Server.Database.Fields (_id)
import Server.Database.Messages (_content)
import Server.Database.Posts (_poster, posts)
import Server.Effect (ServerEffect)
import Shared.Post (Post)
import Shared.Unsafe as SU

presentPosts ∷ Int → Int → Maybe Int → Maybe Int → ServerEffect (Array Post)
presentPosts loggedUserId userId before after = SD.unsafeQuery query { loggedUserId, userId, before, after }
      where
      query = """
            select p.id, p.content, p.date, p.expires
            from posts p
            join users u on p.poster = u.id
            where u.id = @userId
              and ((@before :: integer) is null or p.id < @before)
              and ((@after :: integer) is null or p.id > @after)
              and (
                    u.posts_visibility = 0
                    or u.posts_visibility = 1
                      and not exists (select 1 from users s where s.id = @loggedUserId and s.temporary = true)
                    or u.posts_visibility = 2
                      and exists (
                            select 1
                            from histories h
                            where (h.sender = @loggedUserId and h.recipient = @userId)
                               or (h.recipient = @loggedUserId and h.sender = @userId)
                      )
              )
            order by p.date desc
            limit 8
            """

savePost ∷ Int → String → ServerEffect { id ∷ Int }
savePost loggedUserId content = map SU.fromJust $ SD.single $ insert # into posts (_content /\ _poster) # values (content /\ loggedUserId) # returning _id

markSeen ∷ Int → Int → Int → ServerEffect Unit
markSeen loggedUserId poster id = SD.unsafeExecute "insert into posts_seen (poster, reader, until) values (@poster, @reader, @until) on conflict (poster, reader) do update set until = greatest(posts_seen.until, excluded.until)" { poster, until: id, reader: loggedUserId }