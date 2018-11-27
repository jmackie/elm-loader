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
import Data.DateTime.Instant as Instant
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now
import Node.Buffer (Buffer)
import Node.ChildProcess.Aff as ChildProcess
import Node.FS.Aff as FS
import Node.OS as OS
import Node.Path (FilePath)
import Node.Path as Path


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
    :: Flags
    -> Target
    -> FilePath
    -> Aff (Either String Buffer)
make = makeWith "elm"


makeWith
    :: String
    -> Flags
    -> Target
    -> FilePath
    -> Aff (Either String Buffer)
makeWith elm flags target entrypoint =
    withResource randomOutputFilePath tryUnlink
        (makeWith' elm flags target entrypoint)
  where
    randomOutputFilePath :: Aff FilePath
    randomOutputFilePath = do
        dir  <- liftEffect OS.tmpdir
        nowString <- show <$> now
        let base = "elm-loader-" <> nowString <> "." <> targetToExtension target
        pure (Path.concat [ dir, base ])

    -- NOTE: if `elm` fails then the file won't be created. And if we
    -- try to unlink a non-existent file it will raise an exception.
    tryUnlink :: FilePath -> Aff Unit
    tryUnlink = void <<< Aff.try <<< FS.unlink


makeWith'
    :: String
    -> Flags
    -> Target
    -> FilePath
    -> FilePath
    -> Aff (Either String Buffer)
makeWith' elm flags target entrypoint outputFile = do
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
        ChildProcess.defaultSpawnOptions


flagsToArgs :: Flags -> Array String
flagsToArgs flags =
    Array.catMaybes
        [ if flags.debug    then Just "--debug"    else Nothing
        , if flags.optimize then Just "--optimize" else Nothing
        ]


now :: forall m. MonadEffect m => m Number
now = liftEffect (unwrap <<<  Instant.unInstant <$> Now.now)
