module Node.Process (chdir) where

import Prelude

import Effect (Effect)
import Node.Path (FilePath)


foreign import chdir :: FilePath -> Effect Unit
