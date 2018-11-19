module Language.Elm.Project
    ( Project(App, Pkg)
    , AppInfo
    , PkgInfo
    , Name
    , Version
    )
where

import Prelude

import Data.Argonaut (Json, class DecodeJson, decodeJson, (.?))
import Data.Either (Either(Left), note)
import Data.Int as Int
import Data.String.Common (split) as String
import Data.String.Pattern (Pattern(Pattern)) as String
import Foreign.Object (Object)
import Node.Path (FilePath)


data Project
    = App AppInfo
    | Pkg PkgInfo


instance decodeJsonProject :: DecodeJson Project where
    decodeJson = decodeJson >=> \object ->
        object .? "type" >>= case _ of
            "application" -> App <$> decodeAppInfo object
            "package"     -> Pkg <$> decodePkgInfo object
            other         -> Left ("unknown project type: " <> other)


type AppInfo =
    { elmVersion :: Version
    , sourceDirs :: Array FilePath
    -- , depsDirect :: Map Name Version
    -- , depsTrans  :: Map Name Version
    -- , testDirect :: Map Name Version
    -- , testTrans  :: Map Name Version
    }


decodeAppInfo :: Object Json -> Either String AppInfo
decodeAppInfo object = do
    elmVersion <- object .? "elm-version" >>= decodeVersion
    sourceDirs <- object .? "source-directories"
    pure { elmVersion, sourceDirs }


type PkgInfo =
    { name       :: Name
    , summary    :: String
    , license    :: String
    , version    :: Version
    -- , exposed    :: Exposed
    -- , deps       :: Map Name Constraint
    -- , testDeps   :: Map Name Constraint
    -- , elmVersion :: Constraint
    }


decodePkgInfo :: Object Json -> Either String PkgInfo
decodePkgInfo object = do
    name    <- object .? "name" >>= decodeName
    summary <- object .? "summary"
    license <- object .? "license"
    version <- object .? "version" >>= decodeVersion
    pure { name, summary, license, version }


type Name =
    { author  :: String
    , project :: String
    }


decodeName :: Json -> Either String Name
decodeName = decodeJson >=> \string ->
    case String.split (String.Pattern "/") string of
         [ author, project ] ->
            pure { author, project }
         _ ->
            Left ("bad name: " <> string)


type Version =
    { major :: Int
    , minor :: Int
    , patch :: Int
    }


decodeVersion :: Json -> Either String Version
decodeVersion = decodeJson >=> \string ->
    case String.split (String.Pattern ".") string of
         [ major', minor', patch' ] -> do
            major <- Int.fromString major' # note ("bad major version: " <> major')
            minor <- Int.fromString minor' # note ("bad minor version: " <> minor')
            patch <- Int.fromString patch' # note ("bad patch version: " <> patch')
            pure { major, minor, patch }

         _ ->
            Left ("bad version: " <> string)
