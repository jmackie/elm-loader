module Node.OS (tmpdir) where

import Effect (Effect)
import Node.Path (FilePath)

foreign import tmpdir :: Effect FilePath
