module Client.Im.Notification where

import Prelude
import Shared.Im.Types

import Client.Dom as CCD
import Client.Im.Flame (NextMessage)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame.Subscription as FS
import Shared.Element as SE
import Shared.Im.Unread as SIU
import Client.AppId (imAppId)
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Unsafe as SU
import Web.HTML.HTMLLinkElement as WHL

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
              liftEffect $ notify model userIds
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
            , icon: SP.resourcePath (Left Loading) Png
            , handler: FS.send imAppId $ ResumeChat user.id --move to given chat when clicking on system notification
            }

notify' ∷ ImModel → Array Int → Aff (Maybe ImMessage)
notify' model userIds = do
      liftEffect $ notify model userIds
      pure Nothing

updateTabCount ∷ ImModel → Array Contact → Effect Unit
updateTabCount model contacts = do
      CCD.setTitle $ SIU.title unreadChats
      unless model.smallScreen do
            faviconElement ← CCD.unsafeGetElementById SE.Favicon
            WHL.setHref (SIU.favicon unreadChats) <<< SU.fromJust $ WHL.fromElement faviconElement
      where
      unreadChats = SIU.countUnreadChats model.user.id contacts

checkNotifications ∷ Effect Unit
checkNotifications = do
      status ← CCD.notificationPermission
      when (status == "default") $ FS.send imAppId ToggleAskNotification