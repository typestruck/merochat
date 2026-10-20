module Client.Im.WebSocket.Connection where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView)
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Effect (Effect)
import Partial.Unsafe as PU
import Unsafe.Coerce as UC
import Web.Event.EventTarget (EventTarget)
import Web.File.Blob (Blob)
import Web.Internal.FFI as WIF
import Web.Socket.BinaryType (BinaryType(..))
import Web.Socket.BinaryType as WSB
import Web.Socket.ReadyState (ReadyState)
import Web.Socket.ReadyState as WSR

foreign import data WebSocket ∷ Type

fromEventTarget ∷ EventTarget → Maybe WebSocket
fromEventTarget = WIF.unsafeReadProtoTagged "WebSocket"

toEventTarget ∷ WebSocket → EventTarget
toEventTarget = UC.unsafeCoerce

newtype Protocol = Protocol String

derive newtype instance eqProtocol ∷ Eq Protocol
derive newtype instance ordProtocol ∷ Ord Protocol
derive instance newtypeProtocol ∷ Newtype Protocol _

foreign import create ∷ String → Array Protocol → Effect WebSocket

foreign import url ∷ WebSocket → Effect String

foreign import readyStateImpl ∷ WebSocket → Effect Int

readyState ∷ WebSocket → Effect ReadyState
readyState ws = do
      rs ← readyStateImpl ws
      pure $ PU.unsafePartial $ DM.fromJust $ WSR.toEnumReadyState rs

foreign import bufferedAmount ∷ WebSocket → Effect Number

foreign import extensions ∷ WebSocket → Effect String
foreign import protocol ∷ WebSocket → Effect String

foreign import close ∷ WebSocket → Effect Unit

foreign import getBinaryTypeImpl ∷ WebSocket → Effect String
foreign import setBinaryTypeImpl ∷ WebSocket → String → Effect Unit

getBinaryType ∷ WebSocket → Effect BinaryType
getBinaryType ws = PU.unsafePartial do
      getBinaryTypeImpl ws <#> case _ of
            "blob" → Blob
            "arraybuffer" → ArrayBuffer

setBinaryType ∷ WebSocket → BinaryType → Effect Unit
setBinaryType ws = setBinaryTypeImpl ws <<< WSB.printBinaryType

sendString ∷ WebSocket → String → Effect Unit
sendString = sendImpl

sendBlob ∷ WebSocket → Blob → Effect Unit
sendBlob = sendImpl

sendArrayBuffer ∷ WebSocket → ArrayBuffer → Effect Unit
sendArrayBuffer = sendImpl

sendArrayBufferView ∷ ∀ t. WebSocket → ArrayView t → Effect Unit
sendArrayBufferView = sendImpl

foreign import sendImpl ∷ ∀ a. WebSocket → a → Effect Unit
