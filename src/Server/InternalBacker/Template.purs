module Server.InternalBacker.Template where

import Server.Types
import Shared.Types

import Effect (Effect)
import Flame.Renderer.String as FRS
import Shared.Backer.View as SIBV

template ∷ Effect String
template = FRS.render SIBV.view
