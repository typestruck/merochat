module Client.Lynx.Room
      ( UserEntity
      , ContactEntity
      , class Selectable
      , class Insertable
      , class Entity
      , entityName
      , prepareRows
      , prepareResult
      , insert
      , select
      ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as CP
import Data.Argonaut.Core as DAC
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode as DAE
import Data.Argonaut.Parser as DAP
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Exception as EE
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Shared.Im.Types (Contact)
import Shared.User (User)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

-- Room stores the data as shown to the user, following ImModel types.
-- Selection is inferred from Aff (Maybe User) or Aff (Maybe (Array Contact)).
-- Insertion takes User or Array Contact directly.

foreign import insert_ ∷ EffectFn2 String String (Promise Unit)
foreign import select_ ∷ EffectFn1 String (Promise String)

foreign import data UserEntity ∷ Type
foreign import data ContactEntity ∷ Type

class Selectable (row ∷ Type) (entity ∷ Type) | row → entity

instance TypeEquals (Record fields) User ⇒ Selectable (Record fields) UserEntity

instance TypeEquals (Record fields) Contact ⇒ Selectable (Array (Record fields)) ContactEntity

class (EncodeJson row, DecodeJson row) ⇐ Entity (entity ∷ Type) row | entity → row where
      entityName ∷ Proxy entity → String
      prepareRows ∷ Proxy entity → row → row
      prepareResult ∷ Proxy entity → row → row

instance Entity UserEntity User where
      entityName _ = "user"
      prepareRows _ = identity
      prepareResult _ = identity

instance Entity ContactEntity (Array Contact) where
      entityName _ = "contacts"
      prepareRows _ = map resetTyping
      prepareResult _ = map resetTyping

class Insertable (row ∷ Type) (entity ∷ Type) | row → entity

instance TypeEquals (Record fields) User ⇒ Insertable (Record fields) UserEntity

instance TypeEquals (Record fields) Contact ⇒ Insertable (Array (Record fields)) ContactEntity

insert ∷ ∀ entity row. Insertable row entity ⇒ Entity entity row ⇒ row → Aff Unit
insert rows = insertJson (entityName entity) $ prepareRows entity rows
      where
      entity = Proxy ∷ Proxy entity

select ∷ ∀ entity row. Selectable row entity ⇒ Entity entity row ⇒ Aff (Maybe row)
select = map (prepareResult entity) <$> selectJson (entityName entity)
      where
      entity = Proxy ∷ Proxy entity

insertJson ∷ ∀ a. EncodeJson a ⇒ String → a → Aff Unit
insertJson entity = CP.toAffE <<< EU.runEffectFn2 insert_ entity <<< DAC.stringify <<< DAE.encodeJson

selectJson ∷ ∀ a. DecodeJson a ⇒ String → Aff (Maybe a)
selectJson entity = do
      rows ← CP.toAffE $ EU.runEffectFn1 select_ entity
      decodeRows rows

resetTyping ∷ Contact → Contact
resetTyping contact = contact { typing = false, typingId = Nothing }

decodeRows ∷ ∀ a. DecodeJson a ⇒ String → Aff a
decodeRows rows = case DAP.jsonParser rows of
      Left error → EA.throwError <<< EE.error $ "Invalid Room JSON: " <> error
      Right json → case DAD.decodeJson json of
            Left error → EA.throwError <<< EE.error $ "Invalid Room data: " <> DAD.printJsonDecodeError error
            Right value → pure value
