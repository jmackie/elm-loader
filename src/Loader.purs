module Loader (loader) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (jsonParser, decodeJson)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (foldl, traverse_)
import Data.Options (Options)
import Data.Options as Options
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception as Exception
import Language.Elm.Make (makeWith, defaultFlags, Target(JS)) as Elm
import Language.Elm.Project (Project(App, Pkg), AppInfo, PkgInfo) as Elm
import Node.Buffer (Buffer)
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Webpack.Loader as Webpack


loader :: Webpack.Loader
loader = Webpack.mkAsyncLoader loader'


loader' :: Webpack.LoaderContext -> Buffer -> Aff Webpack.Result
loader' ctx _buffer = do
    options <- getOptions ctx # either panic pure
    readElmJson >>= case _ of
        Elm.App appInfo -> loaderApp options appInfo ctx
        Elm.Pkg pkgInfo -> loaderPkg options pkgInfo ctx


loaderApp :: Options -> Elm.AppInfo -> Webpack.LoaderContext -> Aff Webpack.Result
loaderApp options appInfo ctx = do
    when options.watch do
        { files, dirs } <- gatherElmDependencies appInfo.sourceDirs
        liftEffect (addContextDependencies ctx (appInfo.sourceDirs <> dirs))
        liftEffect (addDependencies ctx files)

    liftEffect options.onCompileBegin
    result <- compile options (Webpack.resourcePath ctx)
    liftEffect options.onCompileFinish

    case result of
         Left err -> do
            liftEffect (Console.log err)  -- TODO: would emitError be nicer here?
            throwError (Exception.error "elm compilation failed")

         Right output ->
            pure { source: output }


loaderPkg :: Options -> Elm.PkgInfo -> Webpack.LoaderContext -> Aff Webpack.Result
loaderPkg options pkgInfo ctx = do
    panic ("elm package project's aren't currently supported")


gatherElmDependencies :: Array FilePath -> Aff FS.WalkResult
gatherElmDependencies sourceDirs =
    foldl FS.appendWalkResult FS.emptyWalkResult <$> traverse FS.walk sourceDirs


compile :: Options -> FilePath -> Aff (Either String Buffer)
compile options entrypoint =
    Elm.makeWith
        options.compiler
        options.cwd
        Elm.defaultFlags
        Elm.JS
        entrypoint


addContextDependencies :: Webpack.LoaderContext -> Array FilePath -> Effect Unit
addContextDependencies ctx =
    traverse_ (flip Webpack.addContextDependency ctx)


addDependencies :: Webpack.LoaderContext -> Array FilePath -> Effect Unit
addDependencies ctx =
    traverse_ (flip Webpack.addDependency ctx)


readElmJson :: Aff Elm.Project
readElmJson = do
    elmJson <- FS.readTextFile Encoding.UTF8 "elm.json"
    decodeElmProject elmJson # either panic pure


decodeElmProject :: String -> Either String Elm.Project
decodeElmProject = jsonParser >=> decodeJson


getOptions :: Webpack.LoaderContext -> Either String Options
getOptions ctx =
    case Webpack.query ctx of
         Left _ ->
            -- Not handling query strings atm
            Right Options.default

         Right object ->
             Options.fromObject object


panic :: forall m a. MonadThrow Exception.Error m => String -> m a
panic = throwError <<< Exception.error
