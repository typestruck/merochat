module Server.InternalBacker.Template where

import Server.Types
import Shared.ContentType

import Effect (Effect)
import Flame.Renderer.String as FRS
import Shared.Backer.View as SIBV

template ∷ Effect String
template = FRS.render SIBV.view
