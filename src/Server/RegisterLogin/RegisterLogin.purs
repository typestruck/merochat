module RegisterLogin where

import HTTPure as H
import Response as R
import HTTPure(ResponseM)

register :: RegisterLogin -> ResponseM
register registration = do
	R.json "ok"

login :: RegisterLogin -> ResponseM
login registration = R.json "ok"