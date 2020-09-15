module Server.Token where

import Prelude
import Server.Types
import Shared.Types

import Data.Array.NonEmpty as DAN
import Data.Either as DE

import Data.Maybe (Maybe(..))
import Data.String.Regex as DSR
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as DSSU
import Effect (Effect)
import Node.Crypto.Hash as NCHA
import Node.Crypto.Hmac as NCH
import Node.Simple.Jwt as NSJ
import Run as R
import Data.Int as DI
import Run.Reader as RR

hashPassword :: String -> ServerEffect String
hashPassword password = do
      { configuration: { salt } } <- RR.ask
      R.liftEffect $ NCH.hex NCHA.SHA512 salt password

createToken :: PrimaryKey -> ServerEffect String
createToken id = do
      { configuration: { tokenSecret } } <- RR.ask
      NSJ.toString <$> (R.liftEffect <<< NSJ.encode tokenSecret NSJ.HS512 $ show id)

userIDFromToken :: String -> String -> Effect (Maybe PrimaryKey)
userIDFromToken secret = map (DE.either (const Nothing) parseInt53) <<< NSJ.decode secret <<< NSJ.fromString
      where parseInt53 input = do
                  --so glad we dont have to do if err != nil
                  matched <- DSR.match (DSSU.unsafeRegex "(Int53 (\\d+))" noFlags) input
                  position <- DAN.index matched 2
                  match <- position
                  DI.fromString match