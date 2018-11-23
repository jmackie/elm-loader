module Data.Options
    ( Options
    , default
    , fromObject
    )
where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Semigroup.Foldable (intercalateMap)
import Foreign (F, Foreign)
import Foreign as Foreign
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Path (FilePath)


-- | Options passed in to the loader.
type Options =
    { compiler :: String          -- elm binary name
    , watch    :: Boolean         -- watch mode?
    , verbose  :: Boolean         -- print status messages?
    , cwd      :: Maybe FilePath  -- directory to run from
    }


default :: Options
default =
    { compiler: "elm"
    , watch: false
    , verbose: false
    , cwd: Nothing
    }


-- | Read in options from a javascript object.
fromObject :: Object Foreign -> Either String Options
fromObject =
    read >>>
    runExcept >>>
    lmap (intercalateMap ": " Foreign.renderForeignError)


read :: Object Foreign -> F Options
read object = do
    compiler <- option default.compiler
        Foreign.readString
        (Object.lookup "compiler" object)

    watch <- option default.watch
        Foreign.readBoolean
        (Object.lookup "watch" object)

    verbose <- option default.verbose
        Foreign.readBoolean
        (Object.lookup "verbose" object)

    cwd <- option default.cwd
        (\value -> Just    <$> Foreign.readString value
               <|> Nothing <$  Foreign.readNullOrUndefined value
        )
        (Object.lookup "cwd" object)

    pure { compiler, watch, verbose, cwd }
  where
    option :: forall a. a -> (Foreign -> F a) -> (Maybe Foreign) -> F a
    option def = maybe (pure def)
