module Server.NotFound.Template where

import Prelude

import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Shared.Template (defaultParameters, externalFooter)
import Shared.Template as ST

template :: Effect String
template = do
	contents <- ST.template defaultParameters { footer = externalFooter, content = content }
	FRS.render contents
	where   content = [ HE.text "Page not found" ]