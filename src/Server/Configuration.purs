module Server.Configuration where

import Prelude
import Server.Types
import Environment(development)

import Data.Int as DI
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.Traversable as DT
import Effect (Effect)
import Effect.Console as EC
import Effect.Exception as EE
import Node.Process as NP
import Shared.Unsafe as SU

-- | Reads environment variables
-- | The process will fail if any of them are missing on production, dummy values will be used for development
readConfiguration :: Effect Configuration
readConfiguration = do
      randomizeProfiles <- falseUnless <$> NP.lookupEnv "RANDOMIZE_PROFILES"
      if development then
            pure {
                  port: 8000,
                  development: true,
                  captchaSecret: "",
                  tokenSecret: "so nice, so nice, I got you",
                  salt: "put it back together",
                  emailUser: "",
                  emailHost: "",
                  emailPassword: "",
                  randomizeProfiles
            }
       else do
            EC.log "Starting production environment"
            port <- parsePort <$> NP.lookupEnv "PORT"
            variables <- DT.traverse getVariable ["CAPTCHA_SECRET", "TOKEN_SECRET_GET", "TOKEN_SECRET_POST", "SALT", "EMAIL_USER", "EMAIL_HOST", "EMAIL_PASSWORD"]
            case variables of
                  [captchaSecret, tokenSecretGET, tokenSecret, salt, emailUser, emailHost, emailPassword] ->
                        pure $ {
                              randomizeProfiles: true,
                              development: false,
                              port,
                              captchaSecret,
                              tokenSecret,
                              salt,
                              emailUser,
                              emailHost,
                              emailPassword
                        }
                  _ -> EE.throw "Wrong number of environment variables"
      where getVariable name = SU.fromJust <$> NP.lookupEnv name
            parsePort value = SU.fromJust do
                  v <- value
                  DI.fromString v

isCLI :: Effect Boolean
isCLI = falseUnless <$> NP.lookupEnv "CLI"

falseUnless :: Maybe String -> Boolean
falseUnless = DM.maybe false (_ == "true")