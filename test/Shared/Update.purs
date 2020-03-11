module Test.Shared.Update where 

import Data.Newtype as DN
import Shared.Types

updateModel model f = DN.over IMModel f model

updateUser user f = DN.over IMUser f user