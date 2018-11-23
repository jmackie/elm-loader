module Data.Options
    ( Options
    , Hook
    , default
    , fromObject
    )
where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Semigroup.Foldable (intercalateMap)
import Effect (Effect)
import Foreign (F, Foreign)
import Foreign as Foreign
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Path (FilePath)
import Unsafe.Coerce (unsafeCoerce)


-- | Options passed in to the loader.
type Options =
    { compiler        :: String          -- elm binary name
    , watch           :: Boolean         -- watch mode?
    , cwd             :: Maybe FilePath  -- directory to run from
    , onCompileBegin  :: Hook            -- run before compilation
    , onCompileFinish :: Hook            -- run after compilation
    }


type Hook = Effect Unit


default :: Options
default =
    { compiler: "elm"
    , watch: false
    , cwd: Nothing
    , onCompileBegin: pure unit
    , onCompileFinish: pure unit
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

    cwd <- option default.cwd
        (Foreign.readString >>> map Just)
        (Object.lookup "cwd" object)

    onCompileBegin <- option default.onCompileBegin
        readHook
        (Object.lookup "onCompileBegin" object)

    onCompileFinish <- option default.onCompileFinish
        readHook
        (Object.lookup "onCompileFinish" object)

    pure { compiler
         , watch
         , cwd
         , onCompileBegin
         , onCompileFinish
         }
  where
    option :: forall a. a -> (Foreign -> F a) -> (Maybe Foreign) -> F a
    option def = maybe (pure def)


readHook :: Foreign -> F Hook
readHook value =
    if valueType == "function"
       then pure (unsafeCoerce value)
       else Foreign.fail (Foreign.TypeMismatch "function" valueType)
  where
    valueType :: String
    valueType = Foreign.typeOf value
