module Client.Help.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Client.Location as CCL
import Client.Navigation as CN
import Client.Ui.Document as CUD
import Client.Ui.Event as CUEV
import Data.Traversable as DT
import Effect (Effect)
import Data.Void (Void)
import Flame (Application)
import Shared.Element (ElementId(..))
import Shared.External as SE
import Shared.External.Types (Page(..))
import Shared.Faq (faq)
import Shared.Help.View as SHV
import Web.DOM.Element as WDE
import Web.Event.EventTarget as WET
import Web.HTML as WH
import Web.HTML.Event.EventTypes (click)
import Web.HTML.Event.HashChangeEvent.EventTypes (hashchange)
import Web.HTML.Window as WHW

-- :(
main ∷ Effect Unit
main = do
      hash ← CCL.hash
      faqLink ← CUD.unsafeGetElementById FaqLink
      termsLink ← CUD.unsafeGetElementById TermsLink
      privacyLink ← CUD.unsafeGetElementById PrivacyLink
      faq ← CUD.unsafeGetElementById Faq
      terms ← CUD.unsafeGetElementById TermsSection
      privacy ← CUD.unsafeGetElementById PrivacySection

      let
            unselectAll = do
                  DT.traverse_ (WDE.setAttribute "class" "entry") [ faqLink, termsLink, privacyLink ]
                  DT.traverse_ (WDE.setAttribute "class" "hidden") [ faq, terms, privacy ]
            select link tab = do
                  unselectAll
                  WDE.setAttribute "class" "entry selected" link
                  WDE.setAttribute "class" "" tab
            showTab =
                  case _ of
                        "#terms" → select termsLink terms
                        "#privacy" → select privacyLink privacy
                        _ → select faqLink faq

      CUEV.addEventListener faqLink click (const (CCL.setHash Faq))
      CUEV.addEventListener termsLink click (const (CCL.setHash TermsSection))
      CUEV.addEventListener privacyLink click (const (CCL.setHash PrivacySection))

      hashListener ← WET.eventListener $ const (CCL.hash >>= showTab)
      window ← WH.window
      WET.addEventListener hashchange hashListener false $ WHW.toEventTarget window

      showTab hash

application ∷ Application Unit Void
application =
      { model: unit
      , view: const $ SE.view CN.navigate ExternalHelpRoot $ [ SHV.view CN.navigate Help faq ]
      , update: \model _ → model /\ []
      , subscribe: []
      }
