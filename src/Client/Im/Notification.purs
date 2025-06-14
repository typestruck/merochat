module Client.Im.Notification where

import Prelude
import Shared.Im.Types

import Client.Common.Dom as CCD
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
import Shared.Options.MountPoint (imId)
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
notify { user: { id: loggedUserId }, contacts, smallScreen } userIds = do
      updateTabCount loggedUserId contacts
  --    unless smallScreen $ DF.traverse_ createNotification' contactUsers
      where
      contactUsers = DA.filter byKeys contacts
      byKeys cnt = DA.any (\id → cnt.user.id == id) userIds

      createNotification' { user } = createNotification
            { body: "New message from " <> user.name
            , icon: SP.resourcePath (Left Loading) Png
            , handler: FS.send imId $ ResumeChat user.id --move to given chat when clicking on system notification
            }

notify' ∷ ImModel → Array Int → Aff (Maybe ImMessage)
notify' model userIds = do
      liftEffect $ notify model userIds
      pure Nothing

updateTabCount ∷ Int → Array Contact → Effect Unit
updateTabCount id contacts = do
      CCD.setTitle $ SIU.title unreadChats
      faviconElement ← CCD.unsafeGetElementById SE.Favicon
      WHL.setHref (SIU.favicon unreadChats) <<< SU.fromJust $ WHL.fromElement faviconElement
      where
      unreadChats = SIU.countUnreadChats id contacts

checkNotifications ∷ Effect Unit
checkNotifications = do
      status ← CCD.notificationPermission
      when (status == "default") $ FS.send imId ToggleAskNotification