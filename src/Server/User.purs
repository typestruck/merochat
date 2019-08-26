module Server.User where

import Unsafe.Coerce as UC
import Shared.Types

toIMUser :: User -> IMUser
toIMUser = UC.unsafeCoerce