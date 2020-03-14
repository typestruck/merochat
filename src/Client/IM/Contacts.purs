module Client.IM.Contacts where

import Prelude
import Shared.Types

import Data.Array ((:), (!!))
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Int53 as DI
import Data.Maybe(Maybe(..))
import Flame (World)
import Effect.Aff(Aff)
import Shared.Newtype as SN

update :: World IMModel IMMessage -> IMModel -> ContactMessage -> Aff IMModel
update _ model =
        case _ of
                ResumeChat index -> resumeChat model index

resumeChat :: IMModel -> Int -> Aff IMModel
resumeChat model index =
        pure <<< SN.updateModel model $ _ {
                suggesting = Nothing,
                chatting = Just index
        }
