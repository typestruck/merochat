module Response where

import HTTPure as H
import HTTPure.Headers as HH
import HTTPure (ResponseM)

html :: String -> ResponseM
html contents = H.ok' headers contents
	where headers = HH.header "Content-Type" "text/html"