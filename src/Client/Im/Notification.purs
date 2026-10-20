module Client.Im.Notification where

import Prelude

import Client.AppId (imAppId)
import Client.Dom as CD
import Client.Im.Flame (NextMessage)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Uncurried (EffectFn1)
import Flame.Subscription as FS
import Shared.Element as SE
import Shared.Im.Types (Contact, ImMessage(..), ImModel)
import Shared.Im.Unread as SIU
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SR
import Shared.Unsafe as SU
import Web.HTML.HTMLLinkElement as WHH

-- this can be seriously cleaned up

type Notification =
      { body ∷ String
      , icon ∷ String
      , handler ∷ Effect Unit -- uses a custom event to let Main know of this
      }

foreign import createNotification_ ∷ EffectFn1 Notification Unit

createNotification ∷ Notification → Effect Unit
createNotification n = pure unit -- EU.runEffectFn1 createNotification_

notifyUnreadChats ∷ ImModel → Array Int → NextMessage
notifyUnreadChats model userIds = model /\
      [ do
              EC.liftEffect $ notify model userIds
              pure $ Just SetDeliveredStatus
      ]

notify ∷ ImModel → Array Int → Effect Unit
notify model userIds = do
      updateTabCount model model.contacts
      --    unless smallScreen $ DF.traverse_ createNotification' contactUsers
      where
      contactUsers = DA.filter byKeys model.contacts
      byKeys cnt = DA.any (\id → cnt.user.id == id) userIds

      createNotification' { user } = createNotification
            { body: "New message from " <> user.name
            , icon: SR.resourcePath (Left Loading) Png
            , handler: FS.send imAppId $ ResumeChat user.id --move to given chat when clicking on system notification
            }

notify' ∷ ImModel → Array Int → Aff (Maybe ImMessage)
notify' model userIds = do
      EC.liftEffect $ notify model userIds
      pure Nothing

updateTabCount ∷ ImModel → Array Contact → Effect Unit
updateTabCount model contacts = do
      CD.setTitle $ SIU.title unreadChats
      unless model.smallScreen do
            faviconElement ← CD.unsafeGetElementById SE.Favicon
            WHH.setHref (SIU.favicon unreadChats) <<< SU.fromJust $ WHH.fromElement faviconElement
      where
      unreadChats = SIU.countUnreadChats model.user.id contacts
