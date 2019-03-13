module Server.Landing.Action where

import HTTPure (ResponseM)
import HTTPure as H

register :: RegisterLogin -> ResponseM
register registerLogin = holes?