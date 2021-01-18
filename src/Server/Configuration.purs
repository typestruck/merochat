module Server.Configuration where

import Prelude
import Server.Types

import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Traversable as DT
import Effect (Effect)
import Effect.Exception as EE
import Environment (development)
import Node.Process as NP
import Shared.Unsafe as SU

-- | Reads environment variables
-- | The process will fail if any of them are missing on production, dummy values will be used for development
readConfiguration :: Effect Configuration
readConfiguration =
      if development then do
            randomizeProfiles <- falseUnless <$> NP.lookupEnv "RANDOMIZE_PROFILES"
            databaseHost <- NP.lookupEnv "DATABASE_HOST"
            pure {
                  port: 8000,
                  databaseHost,
                  captchaSecret: "",
                  tokenSecret: "so nice, so nice, I got you",
                  salt: "put it back together",
                  emailUser: "",
                  emailHost: "",
                  emailPassword: "",
                  randomizeProfiles
            }
       else do
            port <- parsePort <$> NP.lookupEnv "PORT"
            variables <- DT.traverse getVariable ["CAPTCHA_SECRET", "TOKEN_SECRET", "SALT", "EMAIL_USER", "EMAIL_HOST", "EMAIL_PASSWORD", "DATABASE_HOST"]
            case variables of
                  [captchaSecret, tokenSecret, salt, emailUser, emailHost, emailPassword, host] ->
                        pure $ {
                              randomizeProfiles: true,
                              port,
                              databaseHost: Just host,
                              captchaSecret,
                              tokenSecret,
                              salt,
                              emailUser,
                              emailHost,
                              emailPassword
                        }
                  _ -> EE.throw "Wrong number of environment variables"
      where getVariable name = do
                  value <- NP.lookupEnv name
                  case value of
                        Nothing -> EE.throw $ "missing configuration: " <> name
                        Just value -> pure value
            parsePort value = SU.fromJust do
                  v <- value
                  DI.fromString v

isCLI :: Effect Boolean
isCLI = falseUnless <$> NP.lookupEnv "CLI"

storageAuthenticationKey :: Effect String
storageAuthenticationKey = DM.fromMaybe "" <$> NP.lookupEnv "STORAGE_KEY"

falseUnless :: Maybe String -> Boolean
falseUnless = DM.maybe false (_ == "true")

