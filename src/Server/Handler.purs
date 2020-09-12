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
import Server.Help.Handler as SHH
import Server.IM.Handler as SIH
import Server.InternalError.Handler as SIEH
import Server.Landing.Handler as SLH
import Server.Login.Handler as SLGH
import Server.NotFound.Handler as SNH
import Server.Privacy.Handler as SPVH
import Server.Profile.Handler as SPH
import Server.Recover.Handler as SRH
import Server.Settings.Handler as SSH
import Server.Logout.Handler as SLOH
import Server.Terms.Handler as STH
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
            missedMessages: runJSON reading SIH.missedMessages
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
      terms: runHTML reading STH.terms,
      privacy: runHTML reading SPVH.privacy,
      help: runHTML reading SHH.help,
      notFound: runHTML reading SNH.notFound,

      developmentFiles: developmentFiles
}

runHTML :: forall a. ServerReader -> (a -> ServerEffect Html) -> a -> Aff (Either (Response String) Html)
runHTML reading handler input = run `EA.catchError` catch
      where run = R.runBaseAff' <<< RE.catch requestError <<< RR.runReader reading <<< map Right $ handler input
            catch = liftEffect <<< map Left <<< SIEH.internalError <<< EA.message
            requestError ohno = do
                  R.liftEffect do
                        EC.log $ "server error " <> show ohno
                        map Left $ case ohno of
                              BadRequest { reason } -> SIEH.internalError reason
                              InternalError { reason } -> SIEH.internalError reason
                              ExpiredSession -> SLOH.logout $ routes.im.get {}

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
            distBaseFolder = "dist/"
            fullPath = case path of
                  Cons "media" (Cons file Nil) -> clientBaseFolder <> "media/" <> file
                  Cons "media" (Cons "upload" (Cons file Nil)) -> clientBaseFolder <>  "media/upload/" <> file
                  --js files are expected to be named like module.bundle.js
                  -- they are served from webpack output
                  Cons "javascript" (Cons file Nil) -> distBaseFolder <> file
                  Cons folder (Cons file Nil) -> clientBaseFolder <> folder <> "/" <> file
                  _ -> distBaseFolder <> DS.joinWith "/" (DL.toUnfoldable path)