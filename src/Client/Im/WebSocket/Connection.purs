module Client.Im.WebSocket.Connection where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventTarget)
import Web.File.Blob (Blob)
import Web.Internal.FFI (unsafeReadProtoTagged)
import Web.Socket.BinaryType (BinaryType(..), printBinaryType)
import Web.Socket.ReadyState (ReadyState, toEnumReadyState)

foreign import data WebSocket :: Type

fromEventTarget :: EventTarget -> Maybe WebSocket
fromEventTarget = unsafeReadProtoTagged "WebSocket"

toEventTarget :: WebSocket -> EventTarget
toEventTarget = unsafeCoerce

newtype Protocol = Protocol String

derive newtype instance eqProtocol :: Eq Protocol
derive newtype instance ordProtocol :: Ord Protocol
derive instance newtypeProtocol :: Newtype Protocol _

foreign import create :: String -> Array Protocol -> Effect WebSocket

foreign import url :: WebSocket -> Effect String

foreign import readyStateImpl :: WebSocket -> Effect Int

readyState :: WebSocket -> Effect ReadyState
readyState ws = do
  rs <- readyStateImpl ws
  pure $ unsafePartial $ fromJust $ toEnumReadyState rs

foreign import bufferedAmount :: WebSocket -> Effect Number

foreign import extensions :: WebSocket -> Effect String
foreign import protocol :: WebSocket -> Effect String

foreign import close :: WebSocket -> Effect Unit

foreign import getBinaryTypeImpl :: WebSocket -> Effect String
foreign import setBinaryTypeImpl :: WebSocket -> String -> Effect Unit

getBinaryType :: WebSocket -> Effect BinaryType
getBinaryType ws = unsafePartial do
  getBinaryTypeImpl ws <#> case _ of
    "blob" -> Blob
    "arraybuffer" -> ArrayBuffer

setBinaryType :: WebSocket -> BinaryType -> Effect Unit
setBinaryType ws = setBinaryTypeImpl ws <<< printBinaryType

sendString :: WebSocket -> String -> Effect Unit
sendString = sendImpl

sendBlob :: WebSocket -> Blob -> Effect Unit
sendBlob = sendImpl

sendArrayBuffer :: WebSocket -> ArrayBuffer -> Effect Unit
sendArrayBuffer = sendImpl

sendArrayBufferView :: forall t. WebSocket -> ArrayView t -> Effect Unit
sendArrayBufferView = sendImpl

foreign import sendImpl :: forall a. WebSocket -> a -> Effect Unit