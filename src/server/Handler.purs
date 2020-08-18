module Server.Handler where

import Server.Types

import Run as R
import Prelude
import Run.Except as RE
import Run.Reader as RR
import Server.Landing.Handler as SLH
import Server.Login.Handler as SLGH
import Server.IM.Handler as SIH

handlers :: ServerReader -> _
handlers reading = {
      landing: runHandler reading SLH.landing,
      im: runHandler reading SIH.im,
      login: runHandler reading SLGH.login,
      login': runHandler reading SLGH.login'
}

runHandler reading handler =
      R.runBaseAff' <<< RE.catch (const (pure (Html "unit"))) <<< RR.runReader reading <<< handler
