module Server.Token where

import Prelude

import Data.Array ((!!))
import Data.Either as DE
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Node.Buffer as NB
import Node.Crypto.Hmac as NCH
import Node.Encoding (Encoding(..))
import Node.Simple.Jwt as NSJ
import Run as R
import Run.Reader as RR
import Server.Types (ServerEffect)

hashPassword ∷ String → ServerEffect String
hashPassword password = do
      { configuration: { salt } } ← RR.ask
      R.liftEffect do
            key ← NB.fromString salt UTF8
            buffer ← NB.fromString password UTF8
            NCH.createHmac "sha512" key >>= NCH.update buffer >>= NCH.digest >>= NB.toString Hex

createToken ∷ Int → ServerEffect String
createToken id = do
      { configuration: { tokenSecret } } ← RR.ask
      instant ← liftEffect EN.now
      NSJ.toString <$> (R.liftEffect <<< NSJ.encode tokenSecret NSJ.HS512 $ show instant <> "-" <> show id)

userIdFromToken ∷ String → String → Effect (Maybe Int)
userIdFromToken secret = map (DE.either (const Nothing) toId) <<< NSJ.decode secret <<< NSJ.fromString
      where
      toId raw = (DS.split (Pattern "-") raw !! 1) >>= DI.fromString