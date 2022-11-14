module Server.Feedback.Action where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as DS
import Server.Email as SE
import Server.Feedback.Dabatase as SFD
import Server.File as SF
import Server.Response as SR
import Server.Effect (ServerEffect)

sendFeedback :: Int -> String -> Maybe String -> ServerEffect Unit
sendFeedback loggedUserId rawComments screenshot = do
      let comments = DS.trim rawComments
      when (DS.null comments) $ SR.throwBadRequest "Comments must not be empty"
      fileName ← case screenshot of
            Just base64 -> Just <$> SF.saveBase64File base64
            Nothing -> pure Nothing
      id <- SFD.insertFeedback loggedUserId rawComments fileName
      SE.sendEmail "contact@mero.chat" ("[FEEDBACK] from " <> show loggedUserId) $ "select * from feedbacks where id = " <> show id <> ";"
