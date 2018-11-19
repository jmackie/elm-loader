module Node.FS.Aff
    ( readFile
    , readTextFile
    , unlink
    , readdir
    , stat

    -- File walker
    , walk
    , WalkResult
    , emptyWalkResult
    , appendWalkResult
    )
where

import Prelude

import Data.Array as Array
import Data.Foldable (foldM)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.FS.Async as AsyncFS
import Node.FS.Stats (Stats)
import Node.FS.Stats as Stats
import Node.Path (FilePath)
import Node.Path as Path


readFile :: FilePath -> Aff Buffer
readFile filePath = Aff.makeAff \callback -> do
    AsyncFS.readFile filePath callback
    pure Aff.nonCanceler


readTextFile :: Encoding -> FilePath -> Aff String
readTextFile encoding filePath = Aff.makeAff \callback -> do
    AsyncFS.readTextFile encoding filePath callback
    pure Aff.nonCanceler


unlink :: FilePath -> Aff Unit
unlink filePath = Aff.makeAff \callback -> do
    AsyncFS.unlink filePath callback
    pure Aff.nonCanceler


readdir :: FilePath -> Aff (Array FilePath)
readdir filePath = Aff.makeAff \callback -> do
    AsyncFS.readdir filePath callback
    pure Aff.nonCanceler


stat :: FilePath -> Aff Stats
stat filePath = Aff.makeAff \callback -> do
    AsyncFS.stat filePath callback
    pure Aff.nonCanceler


type WalkResult =
    { files :: Array FilePath
    , dirs  :: Array FilePath
    }


emptyWalkResult :: WalkResult
emptyWalkResult = { files: [], dirs: [] }


appendWalkResult :: WalkResult -> WalkResult -> WalkResult
appendWalkResult lhs rhs =
    { files: lhs.files <> rhs.files
    , dirs:  lhs.dirs  <> rhs.dirs
    }


walk :: FilePath -> Aff WalkResult
walk = go emptyWalkResult
  where
    go :: WalkResult -> FilePath -> Aff WalkResult
    go walkResult dir = do
       filePaths <- readdir dir
       foldM processFilePath walkResult (pathJoin dir <$> filePaths)

    pathJoin :: FilePath -> FilePath -> FilePath
    pathJoin dir base = Path.concat [ dir, base ]

    processFilePath :: WalkResult -> FilePath -> Aff WalkResult
    processFilePath accum filePath = do
        stats <- stat filePath
        if Stats.isDirectory stats
           then appendWalkResult (addDir filePath accum) <$> walk filePath
           else pure (addFile filePath accum)

    addFile :: FilePath -> WalkResult -> WalkResult
    addFile file walkResult =
        walkResult { files = walkResult.files `Array.snoc` file }

    addDir :: FilePath -> WalkResult -> WalkResult
    addDir dir walkResult =
        walkResult { dirs = walkResult.dirs `Array.snoc` dir }
