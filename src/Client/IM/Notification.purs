module Client.IM.Notification where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.IM.Flame (NextMessage)
import Client.IM.Flame as CIF
import Data.Array as DA
import Data.Foldable as DF
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Shared.IM.Unread as SIU
import Shared.Unsafe as SU
import Web.HTML.HTMLLinkElement as WHL

foreign import createNotification_ :: EffectFn1 String Unit

createNotification :: String -> Effect Unit
createNotification = EU.runEffectFn1 createNotification_

notifyUnreadChats :: IMModel -> Array PrimaryKey -> NextMessage
notifyUnreadChats model userIDs = CIF.nothingNext model <<< liftEffect $ notify model userIDs

notify :: IMModel -> Array PrimaryKey -> Effect Unit
notify model@{ user: { id }, contacts } userIDs = do
      updateTabCount id contacts
      DF.traverse_ createNotification contactNames
      where contactNames = map (_.name <<< _.user) $ DA.filter (\cnt -> DA.elem cnt.user.id userIDs) contacts

updateTabCount :: PrimaryKey -> Array Contact -> Effect Unit
updateTabCount id contacts = do
      CCD.setTitle $ SIU.title unreadChats
      faviconElement <- CCD.unsafeQuerySelector "#favicon"
      WHL.setHref (SIU.favicon unreadChats) <<< SU.fromJust $ WHL.fromElement faviconElement
      where unreadChats = SIU.countUnreadChats id contacts




