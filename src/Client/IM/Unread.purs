module Client.IM.Unread where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.IM.Flame (NextMessage)
import Client.IM.Flame as CIF
import Effect (Effect)
import Effect.Class (liftEffect)
import Shared.IM.Unread as SIU
import Shared.Unsafe as SU
import Web.HTML.HTMLLinkElement as WHL

alertUnreadChats :: IMModel -> NextMessage
alertUnreadChats model@{ user: { id }, contacts} = CIF.nothingNext model <<< liftEffect $ setTabCount id contacts

setTabCount :: PrimaryKey -> Array Contact -> Effect Unit
setTabCount id contacts = do
      let unreadChats = SIU.countUnreadChats id contacts
      CCD.setTitle $ SIU.title unreadChats
      faviconElement <- CCD.querySelector "#favicon"
      WHL.setHref (SIU.favicon unreadChats) <<< SU.fromJust $ WHL.fromElement faviconElement

