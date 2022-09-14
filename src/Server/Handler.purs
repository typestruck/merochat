module Server.Handler where

import Prelude
import Server.Types

import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as DL
import Data.String as DS
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Class.Console as EC
import Payload.ResponseTypes (Response)
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as PSH
import Payload.Server.Response as PSR
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.Backer.Handler as SBH
import Server.Experiments.Handler as SEH
import Server.Fortune.Handler as SFTH
import Server.Help.Handler as SHH
import Server.Im.Handler as SIH
import Server.InternalBacker.Handler as SIBH
import Server.InternalError.Handler as SIEH
import Server.InternalHelp.Handler as SIHH
import Server.Landing.Handler as SLH
import Server.Leaderboard.Handler as SLBH
import Server.Login.Handler as SLGH
import Server.Logout as SL
import Server.Logout.Handler as SLOH
import Server.NotFound.Handler as SNH
import Server.Profile.Handler as SPH
import Server.Recover.Handler as SRH
import Server.Settings.Handler as SSH
import Shared.ResponseError (ResponseError(..))
import Shared.Routes (routes)

handlers ∷ ServerReader → _
handlers reading =
      { landing: runHtml reading SLH.landing
      , register: runJson reading SLH.register
      , temporary: runJson reading SLH.temporary
      , im:
              { get: runHtml reading SIH.im
              , contacts: runJson reading SIH.contacts
              , contact: runJson reading SIH.contact
              , history: runJson reading SIH.history
              , suggestions: runJson reading SIH.suggestions
              , block: runJson reading SIH.block
              , delete: runJson reading SIH.deleteChat
              , missedEvents: runJson reading SIH.missedEvents
              , fortune: runJson reading SFTH.fortune
              , register: runJson reading SIH.register
              , report: runJson reading SIH.report
              , tutorial: runJson reading SIH.tutorial
              }
      , profile:
              { get: runJson reading SPH.profile
              , field:
                      { generated: runJson reading SPH.generated
                      , avatar: runJson reading SPH.avatar
                      , age: runJson reading SPH.age
                      , gender: runJson reading SPH.gender
                      , country: runJson reading SPH.country
                      , language: runJson reading SPH.language
                      , tag: runJson reading SPH.tag
                      }
              }
      , login:
              { get: runHtml reading SLGH.login
              , post: runJson reading SLGH.logon
              }
      , settings:
              { get: runJson reading SSH.settings
              , account:
                      { email: runJson reading SSH.accountEmail
                      , password: runJson reading SSH.accountPassword
                      , terminate: runJson reading SSH.accountTerminate
                      , privacy: runJson reading SSH.changePrivacy
                      }
              }
      , recover:
              { get: runHtml reading SRH.recover
              , post: runJson reading SRH.recoverAccount
              , reset: runJson reading SRH.reset
              }
      , internalHelp: runJson reading SIHH.internalHelp
      , leaderboard: runJson reading SLBH.leaderboard
      , logout: runJson reading SLOH.logout
      , help: runHtml reading SHH.help
      , backer: runHtml reading SBH.backer
      , internalBacker: runJson reading SIBH.internalBacker
      , experiments: runJson reading SEH.experiments
      , developmentFiles: developmentFiles
      , notFound: runHtml reading SNH.notFound
      }

runHtml ∷ ∀ a b. ServerReader → (a → ServerEffect b) → a → Aff (Either (Response String) b)
runHtml reading handler input = run `EA.catchError` catch
      where
      run = R.runBaseAff' <<< RE.catch requestError <<< RR.runReader reading <<< map Right $ handler input
      catch = liftEffect <<< map Left <<< SIEH.internalError <<< EA.message
      requestError ohno = do
            R.liftEffect do
                  EC.log $ "server error " <> show ohno
                  map Left $ case ohno of
                        BadRequest { reason } → SIEH.internalError reason
                        InternalError { reason } → SIEH.internalError reason
                        ExpiredSession → pure $ SL.logout (routes.login.get {}) ""

runJson ∷ ∀ a b. ServerReader → (a → ServerEffect b) → a → Aff (Either (Response String) b)
runJson reading handler =
      R.runBaseAff' <<< RE.catch requestError <<< RR.runReader reading <<< map Right <<< handler
      where
      requestError ohno = do
            R.liftEffect <<< EC.log $ "server error " <> show ohno
            pure <<< Left $ case ohno of
                  BadRequest { reason } → PSR.badRequest reason
                  InternalError { reason } → PSR.internalError reason
                  ExpiredSession → PSR.unauthorized ""

developmentFiles ∷ { params ∷ { path ∷ List String } } → Aff File
developmentFiles { params: { path } } = PSH.file fullPath {}
      where
      clientBaseFolder = "src/Client/"
      distBaseFolder = "dist/development/"
      fullPath = case path of
            Cons "media" (Cons file Nil) → clientBaseFolder <> "media/" <> file
            Cons "media" (Cons "upload" (Cons file Nil)) → clientBaseFolder <> "media/upload/" <> file
            --js files are expected to be named like module.bundle.js
            -- they are served from webpack output
            Cons "javascript" (Cons file Nil) → distBaseFolder <> file
            Cons folder (Cons file Nil) → clientBaseFolder <> folder <> "/" <> file
            _ → distBaseFolder <> DS.joinWith "/" (DL.toUnfoldable path)