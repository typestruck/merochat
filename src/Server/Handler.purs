module Server.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as DL
import Data.Maybe (Maybe(..))
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
import Server.IM.Handler as SIH
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
import Shared.Routes (routes)

handlers :: ServerReader -> _
handlers reading = {
      landing: runHTML reading SLH.landing,
      register: runJSON reading SLH.register,
      im: {
            get: runHTML reading SIH.im,
            contacts: runJSON reading SIH.contacts,
            singleContact: runJSON reading SIH.singleContact,
            history: runJSON reading SIH.history,
            suggestions: runJSON reading SIH.suggestions,
            block: runJSON reading SIH.block,
            missedEvents: runJSON reading SIH.missedEvents,
            fortune: runJSON reading SFTH.fortune,
            report: runJSON reading SIH.report
      },
      profile: {
            get: runJSON reading SPH.profile,
            post: runJSON reading SPH.profileUpdate,
            generate: runJSON reading SPH.generate
      },
      login: {
            get: runHTML reading SLGH.login,
            post: runJSON reading SLGH.logon
      },
      settings: {
            get: runJSON reading SSH.settings,
            account: {
                  email: runJSON reading SSH.accountEmail,
                  password: runJSON reading SSH.accountPassword,
                  terminate: runJSON reading SSH.accountTerminate
            }
      },
      recover: {
            get: runHTML reading SRH.recover,
            post: runJSON reading SRH.recoverAccount,
            reset : runJSON reading SRH.reset
      },
      internalHelp: runJSON reading SIHH.internalHelp,
      leaderboard: runJSON reading SLBH.leaderboard,
      logout: runJSON reading SLOH.logout,
      help: runHTML reading SHH.help,
      backer: runHTML reading SBH.backer,
      internalBacker: runJSON reading SIBH.internalBacker,
      experiments: runJSON reading SEH.experiments,

      developmentFiles: developmentFiles,

      notFound: runHTML reading SNH.notFound
}

runHTML :: forall a b. ServerReader -> (a -> ServerEffect b) -> a -> Aff (Either (Response String) b)
runHTML reading handler input = run `EA.catchError` catch
      where run = R.runBaseAff' <<< RE.catch requestError <<< RR.runReader reading <<< map Right $ handler input
            catch = liftEffect <<< map Left <<< SIEH.internalError <<< EA.message
            requestError ohno = do
                  R.liftEffect do
                        EC.log $ "server error " <> show ohno
                        map Left $ case ohno of
                              BadRequest { reason } -> SIEH.internalError reason
                              InternalError { reason } -> SIEH.internalError reason
                              ExpiredSession -> pure $ SL.logout (routes.login.get {}) ""

runJSON :: forall a b. ServerReader -> (a -> ServerEffect b) -> a -> Aff (Either (Response String) b)
runJSON reading handler =
      R.runBaseAff' <<< RE.catch requestError <<< RR.runReader reading <<< map Right <<< handler
      where requestError ohno = do
                  R.liftEffect <<< EC.log $ "server error " <> show ohno
                  pure <<< Left $ case ohno of
                        BadRequest { reason } -> PSR.badRequest reason
                        InternalError { reason } -> PSR.internalError reason
                        ExpiredSession -> PSR.unauthorized ""

developmentFiles :: { params :: { path :: List String } } -> Aff File
developmentFiles { params: { path } } = PSH.file fullPath {}
      where clientBaseFolder = "src/Client/"
            distBaseFolder = "dist/development/"
            fullPath = case path of
                  Cons "media" (Cons file Nil) -> clientBaseFolder <> "media/" <> file
                  Cons "media" (Cons "upload" (Cons file Nil)) -> clientBaseFolder <>  "media/upload/" <> file
                  --js files are expected to be named like module.bundle.js
                  -- they are served from webpack output
                  Cons "javascript" (Cons file Nil) -> distBaseFolder <> file
                  Cons folder (Cons file Nil) -> clientBaseFolder <> folder <> "/" <> file
                  _ -> distBaseFolder <> DS.joinWith "/" (DL.toUnfoldable path)