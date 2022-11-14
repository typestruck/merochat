module Server.InternalBacker.Template where

import Server.Effect

import Effect (Effect)
import Flame.Renderer.String as FRS
import Shared.Backer.View as SIBV

template ∷ Effect String
template = FRS.render SIBV.view
