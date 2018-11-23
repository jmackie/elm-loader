module Data.Options
    ( Options
    , Hook
    , defaults
    , fromObject
    )
where

import Prelude

import Control.Monad.Except (runExcept, withExcept)
import Data.Bifunctor (lmap)
import Data.NonEmpty (NonEmpty(NonEmpty))
import Data.List.Types (NonEmptyList(NonEmptyList))
import Data.Either (Either)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Semigroup.Foldable (intercalateMap)
import Data.Tuple (Tuple(Tuple), fst, snd)
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
    , debug           :: Boolean         -- enable the elm debug flag
    , optimize        :: Boolean         -- enable the elm optimize flag
    , watch           :: Boolean         -- watch mode?
    , cwd             :: Maybe FilePath  -- directory to run from
    , onCompileBegin  :: Hook            -- run before compilation
    , onCompileFinish :: Hook            -- run after compilation
    }


type Hook = Effect Unit


defaults :: Options
defaults =
    { compiler: "elm"
    , debug: false
    , optimize: false
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
    Tuple options object' <-
        foldM applyOptionReader (Tuple defaults object) optionReaders
    case Object.keys object' of
         [] -> pure options
         keys -> Foreign.fail $
            Foreign.ForeignError ("Passed unexpected options: " <> show keys)
  where
    applyOptionReader
        :: Tuple Options (Object Foreign)
        -> OptionReader
        -> F (Tuple Options (Object Foreign))
    applyOptionReader accum optionReader =
        optionReader (fst accum) (snd accum)

    optionReaders :: Array OptionReader
    optionReaders =
        [ "compiler"        # compilerOption
        , "debug"           # debugOption
        , "optimize"        # optimizeOption
        , "watch"           # watchOption
        , "cwd"             # cwdOption
        , "onCompileBegin"  # onCompileBeginOption
        , "onCompileFinish" # onCompileFinishOption
        ]


type OptionReader =
    Options -> Object Foreign -> F (Tuple Options (Object Foreign))


compilerOption :: String -> OptionReader
compilerOption key options object =
    withExcept (mapHead (Foreign.ErrorAtProperty key))
        case Object.pop key object of
             Nothing -> pure (Tuple options object)
             Just (Tuple option object') -> do
                compiler <- Foreign.readString option
                pure (Tuple options { compiler = compiler } object')


debugOption :: String -> OptionReader
debugOption key options object =
    withExcept (mapHead (Foreign.ErrorAtProperty key))
        case Object.pop key object of
             Nothing -> pure (Tuple options object)
             Just (Tuple option object') -> do
                debug <- Foreign.readBoolean option
                pure (Tuple options { debug = debug } object')


optimizeOption :: String -> OptionReader
optimizeOption key options object =
    withExcept (mapHead (Foreign.ErrorAtProperty key))
        case Object.pop key object of
             Nothing -> pure (Tuple options object)
             Just (Tuple option object') -> do
                optimize <- Foreign.readBoolean option
                pure (Tuple options { optimize = optimize } object')


watchOption :: String -> OptionReader
watchOption key options object =
    withExcept (mapHead (Foreign.ErrorAtProperty key))
        case Object.pop key object of
             Nothing -> pure (Tuple options object)
             Just (Tuple option object') -> do
                watch <- Foreign.readBoolean option
                pure (Tuple options { watch = watch } object')


cwdOption :: String -> OptionReader
cwdOption key options object =
    withExcept (mapHead (Foreign.ErrorAtProperty key))
        case Object.pop key object of
             Nothing -> pure (Tuple options object)
             Just (Tuple option object') -> do
                cwd <- Foreign.readString option
                pure (Tuple options { cwd = Just cwd } object')


onCompileBeginOption :: String -> OptionReader
onCompileBeginOption key options object =
    withExcept (mapHead (Foreign.ErrorAtProperty key))
        case Object.pop key object of
             Nothing -> pure (Tuple options object)
             Just (Tuple option object') -> do
                onCompileBegin <- readHook option
                pure (Tuple options { onCompileBegin = onCompileBegin } object')


onCompileFinishOption :: String -> OptionReader
onCompileFinishOption key options object =
    withExcept (mapHead (Foreign.ErrorAtProperty key))
        case Object.pop key object of
             Nothing -> pure (Tuple options object)
             Just (Tuple option object') -> do
                onCompileFinish <- readHook option
                pure (Tuple options { onCompileFinish = onCompileFinish } object')



mapHead :: forall a. (a -> a) -> NonEmptyList a -> NonEmptyList a
mapHead f (NonEmptyList (NonEmpty head tail)) =
    NonEmptyList (NonEmpty (f head) tail)


readHook :: Foreign -> F Hook
readHook value =
    if valueType == "function"
       then pure (unsafeCoerce value)
       else Foreign.fail (Foreign.TypeMismatch "function" valueType)
  where
    valueType :: String
    valueType = Foreign.typeOf value
