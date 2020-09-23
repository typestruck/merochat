module Client.IM.Unread where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Effect (Effect)
import Shared.IM.Unread as SIU
import Shared.Unsafe as SU
import Web.HTML.HTMLLinkElement as WHL

alertUnreadChats :: IMModel -> Effect Unit
alertUnreadChats { user: { id }, contacts} = do
      let unreadChats = SIU.countUnreadChats id contacts
      CCD.setTitle $ SIU.title unreadChats
      faviconElement <- CCD.querySelector "#favicon"
      WHL.setHref (SIU.favicon unreadChats) <<< SU.fromJust $ WHL.fromElement faviconElement

