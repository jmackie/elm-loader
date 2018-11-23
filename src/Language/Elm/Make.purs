module Language.Elm.Make
    ( make
    , makeWith
    , Flags
    , defaultFlags
    , Target(HTML, JS)
    )

where

import Prelude

import Control.Monad.Error.Class (withResource)
import Data.Array as Array
import Data.Either (Either(Left, Right))
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Node.Buffer (Buffer)
import Node.ChildProcess.Aff as ChildProcess
import Node.FS.Aff as FS
import Node.Path (FilePath)


type Flags =
    { debug    :: Boolean
    , optimize :: Boolean
    }


defaultFlags :: Flags
defaultFlags = { debug: false, optimize: false }


data Target
    = HTML
    | JS


targetToExtension :: Target -> String
targetToExtension = case _ of
    HTML -> "html"
    JS   -> "js"


make
    :: Maybe FilePath
    -> Flags
    -> Target
    -> FilePath
    -> Aff (Either String Buffer)
make = makeWith "elm"


makeWith
    :: String
    -> Maybe FilePath
    -> Flags
    -> Target
    -> FilePath
    -> Aff (Either String Buffer)
makeWith elm cwd flags target entrypoint =
    withResource randomOutputFilePath tryUnlink
        (makeWith' elm cwd flags target entrypoint)
  where
    randomOutputFilePath :: Aff FilePath
    randomOutputFilePath = addExtension (targetToExtension target) <$> randomHash

    -- NOTE: if `elm` fails then the file won't be created. And if we
    -- try to unlink a non-existent file it will raise an exception.
    tryUnlink :: FilePath -> Aff Unit
    tryUnlink = void <<< Aff.try <<< FS.unlink


makeWith'
    :: String
    -> Maybe FilePath
    -> Flags
    -> Target
    -> FilePath
    -> FilePath
    -> Aff (Either String Buffer)
makeWith' elm cwd flags target entrypoint outputFile = do
    result <- ChildProcess.spawn elm args spawnOptions
    case result of
         { exit: ChildProcess.Normally 0 } -> Right <$> FS.readFile outputFile
         { stderr } -> pure (Left stderr)
  where
    args :: Array String
    args =
        [ "make" ] <> flagsToArgs flags <> [ "--output", outputFile, entrypoint ]

    spawnOptions :: ChildProcess.SpawnOptions
    spawnOptions =
        ChildProcess.defaultSpawnOptions { cwd = cwd }


flagsToArgs :: Flags -> Array String
flagsToArgs flags =
    Array.catMaybes
        [ if flags.debug    then Just "--debug"    else Nothing
        , if flags.optimize then Just "--optimize" else Nothing
        ]


type Hash = String


randomHash :: forall m. MonadEffect m => m Hash
randomHash = liftEffect
    (Int.toStringAs Int.hexadecimal <$> randomInt 1000 10000)


addExtension :: String -> FilePath -> FilePath
addExtension ext filePath = filePath <> "." <> ext
