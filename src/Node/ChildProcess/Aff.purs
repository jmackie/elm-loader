module Node.ChildProcess.Aff
    ( Result
    , spawn

    , module Reexports
    )
where

import Prelude

import Data.Either (Either(Left, Right))
import Data.Posix.Signal as Signal
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Node.ChildProcess as ChildProcess
import Node.Encoding as Encoding
import Node.Stream as Stream

import Node.ChildProcess (Exit(..), SpawnOptions, defaultSpawnOptions) as Reexports


type Result =
      { stdout :: String
      , stderr :: String
      , exit   :: ChildProcess.Exit
      }


spawn :: String -> Array String -> ChildProcess.SpawnOptions -> Aff Result
spawn cmd args spawnOptions = Aff.makeAff \callback -> do
    stdoutRef <- Ref.new ""
    stderrRef <- Ref.new ""
    process   <- ChildProcess.spawn cmd args spawnOptions

    Stream.onDataString (ChildProcess.stdout process) Encoding.UTF8
        \string -> Ref.modify_ (_ <> string) stdoutRef

    Stream.onDataString (ChildProcess.stderr process) Encoding.UTF8
        \string -> Ref.modify_ (_ <> string) stderrRef

    ChildProcess.onError process
       \error -> callback (Left (ChildProcess.toStandardError error))

    ChildProcess.onExit process
        \exit -> do
            stdout <- Ref.read stdoutRef
            stderr <- Ref.read stderrRef
            callback (Right { stdout, stderr, exit })

    let cancel = void (ChildProcess.kill Signal.SIGTERM process)
    pure (Aff.effectCanceler cancel)
