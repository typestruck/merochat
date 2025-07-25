module Server.Effect where

import Droplet.Driver (Pool)
import Run (AFF, Run, EFFECT)
import Prelude
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Shared.ResponseError (ResponseError)
import Type.Row (type (+))
import Effect.Console as EC
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Effect.Class (liftEffect)

type BaseReader extension =
      { pool ∷ Pool
      | extension
      }

type ServerReader = BaseReader
      (
      )

type BaseEffect r a = Run (READER r + EXCEPT ResponseError + AFF + EFFECT + ()) a

type ServerEffect a = BaseEffect ServerReader a

poolEffect pool d = R.runBaseAff' <<< RE.catch (\e → liftEffect (EC.logShow e) *> pure d) <<< RR.runReader { pool }