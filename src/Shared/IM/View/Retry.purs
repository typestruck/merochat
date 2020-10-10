module Shared.IM.View.Retry where

import Prelude
import Shared.Types

import Data.Foldable as DA
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE

retry :: String -> RetryableRequest -> Array RequestFailure -> Html IMMessage
retry failedText requestMessage failedRequests = HE.div [HA.class' { retry: true, invisible: not $ DA.any ((requestMessage == _) <<< _.request) failedRequests }] [
      HE.text failedText,
      HE.span [HA.class' "retry-button", HA.onClick $ SpecialRequest requestMessage] [
            HE.text "Retry",
            HE.svg [HA.class' "svg-16", HA.viewBox "0 0 512 512"] [
                  HE.circle' [HA.cx "256", HA.cy "256", HA.r "240", HA.opacity "0.5"],
                  HE.path' [HA.d "M332,327.128V211.657a114.5,114.5,0,1,0-229,0V295h42V211.657a72.5,72.5,0,0,1,145,0V327.724l-58.147-58.147-29.7,29.7L310.7,407.823,419.249,299.276l-29.7-29.7Z"]
            ]
      ]
]