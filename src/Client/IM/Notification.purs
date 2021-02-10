module Client.IM.Notification where

import Prelude
import Shared.Types

import Client.Common.DOM (notificationClick)
import Client.Common.DOM as CCD
import Client.IM.Flame (NextMessage)
import Client.IM.Flame as CIF
import Data.Array as DA
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Shared.IM.Unread as SIU
import Shared.Path as SP
import Shared.Unsafe as SU
import Web.HTML.HTMLLinkElement as WHL

type Notification = {
      body :: String,
      icon :: String,
      -- uses a custom event to let Main know of this
      handler :: Effect Unit
}

foreign import createNotification_ :: EffectFn1 Notification Unit

createNotification :: Notification -> Effect Unit
createNotification = EU.runEffectFn1 createNotification_

notifyUnreadChats :: IMModel -> Array PrimaryKey -> NextMessage
notifyUnreadChats model userIDs = CIF.nothingNext model <<< liftEffect $ notify model userIDs

notify :: IMModel -> Array PrimaryKey -> Effect Unit
notify model@{ user: { id }, contacts } userIDs = do
      updateTabCount id contacts
      DF.traverse_  createNotification' contactUsers
      where contactUsers = map _.user $ DA.filter (\cnt -> DA.elem cnt.user.id userIDs) contacts
            createNotification' user = createNotification {
                  body: "New message from " <> user.name,
                  icon: SP.pathery PNG "loading",
                  handler: CCD.dispatchCustomEvent $ CCD.createCustomEvent notificationClick user.id
            }

notify' :: IMModel -> Array PrimaryKey -> Aff (Maybe IMMessage)
notify' model userIDs = do
      liftEffect $ notify model userIDs
      pure Nothing

updateTabCount :: PrimaryKey -> Array Contact -> Effect Unit
updateTabCount id contacts = do
      CCD.setTitle $ SIU.title unreadChats
      faviconElement <- CCD.unsafeGetElementByID Favicon
      WHL.setHref (SIU.favicon unreadChats) <<< SU.fromJust $ WHL.fromElement faviconElement
      where unreadChats = SIU.countUnreadChats id contacts