module Server.Token where

import Debug
import Prelude
import Server.Effect

import Data.Array ((!!))
import Data.Either as DE
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Droplet.Driver (Pool)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Node.Buffer as NB
import Node.Crypto.Hmac as NCH
import Node.Encoding (Encoding(..))
import Node.Simple.Jwt as NSJ
import Run as R
import Server.Database.Tokens as SDT
import Server.Effect (ServerEffect)
import Server.Environment (salt, tokenSecret)

hashPassword ∷ String → ServerEffect String
hashPassword password = do
      R.liftEffect do
            key ← NB.fromString salt UTF8
            buffer ← NB.fromString password UTF8
            NCH.createHmac "sha512" key >>= NCH.update buffer >>= NCH.digest >>= NB.toString Hex

createToken ∷ Int → ServerEffect String
createToken id = do
      instant ← liftEffect EN.now
      token ← NSJ.toString <$> (R.liftEffect <<< NSJ.encode tokenSecret NSJ.HS512 $ show instant <> "-" <> show id)
      SDT.insertToken id token
      pure token

userIdFromToken ∷ ∀ r. String → String → BaseEffect { pool ∷ Pool | r } (Maybe Int)
userIdFromToken secret token = do
      decoded ← map (DE.either (const Nothing) toId) <<< liftEffect <<< NSJ.decode secret $ NSJ.fromString token
      case decoded of
            Nothing → pure Nothing
            Just id → do
                  doesIt ← SDT.tokenExists id token
                  if doesIt then
                        pure $ Just id
                  else
                        pure Nothing
      where
      toId raw = (DS.split (Pattern "-") raw !! 1) >>= DI.fromString
