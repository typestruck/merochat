-- | Basic functions to compose templates.
module Template where

import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Prelude(($))

template { script, css, content  }=
	html ! lang "en" $ do
		head $ do
			meta ! charset "UTF-8"
			meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
			link ! rel "shortcut icon" ! type' "image/ico" ! href "/media/favicon.ico"
			link ! rel "stylesheet" ! type' "text/css" href ! "/css/base.css"
    			title $ text "MelanChat (friendly) random webchat"
	body $ do
	    div ! class' "content" $ do
	        header $ do
	            a ! href "/" ! class' "logo" $ text "MelanChat"
	            div class' ! "login" $
	                a ! href "/login" $ text "Login"
	        main content
	        div ! id "loading" ! class' "loading"
	    footer
	        a ! href "/" $ img ! src "/media/logo.png"
	        ul $ do
	            li $ a ! href "#" $ text "Help"
	            li $ a ! href "https://github.com/azafeh/melanchat" $ text "Source code"
	            li $ a ! href "#" $ text "Become a backer"
	            li $ a ! href "/login" $ text "Login"