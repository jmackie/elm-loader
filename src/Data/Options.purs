module Data.Options
    ( Options
    , default
    , fromObject
    )
where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (maybe)
import Data.Semigroup.Foldable (intercalateMap)
import Foreign (F, Foreign)
import Foreign as Foreign
import Foreign.Object (Object)
import Foreign.Object as Object


-- | Options passed in to the loader.
type Options =
    { compiler :: String  -- elm binary name
    , watch    :: Boolean -- watch mode?
    , verbose  :: Boolean -- print status messages?
    }


default :: Options
default =
    { compiler: "elm"
    , watch: false
    , verbose: false
    }


-- | Read in options from a javascript object.
fromObject :: Object Foreign -> Either String Options
fromObject =
    read >>>
    runExcept >>>
    lmap (intercalateMap ": " Foreign.renderForeignError)


read :: Object Foreign -> F Options
read obj = do
    compiler <- maybe (pure default.compiler) Foreign.readString
        (Object.lookup "compiler" obj)

    watch <- maybe (pure default.watch) Foreign.readBoolean
        (Object.lookup "watch" obj)

    verbose <- maybe (pure default.verbose) Foreign.readBoolean
        (Object.lookup "verbose" obj)

    pure { compiler, watch, verbose }
