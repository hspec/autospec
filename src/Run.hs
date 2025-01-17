{-# LANGUAGE CPP #-}
module Run (
  run
, runWeb
#ifdef TEST
, RunArgs(..)
, runWith
, defaultRunArgs
, watchFiles
#endif
) where

import           Imports

import qualified Data.ByteString as ByteString
import           Data.IORef
import           System.IO
import qualified System.FSNotify as FSNotify

import qualified HTTP
import qualified Session

import           EventQueue
import           Trigger
import qualified Input
import           Pager (pager)
import           Util
import           Config
import           GHC.Diagnostic

waitForever :: IO ()
waitForever = forever $ threadDelay 10000000

watchFiles :: FilePath -> EventQueue -> IO () -> IO ()
watchFiles dir queue action = do
  FSNotify.withManager $ \ manager -> do
    bracket (FSNotify.watchTree manager dir isInteresting dispatch) (\ stopListening -> stopListening) $ \ _ -> do
      action
  where
    dispatch :: FSNotify.Event -> IO ()
    dispatch = \ case
      FSNotify.Added file _ _ -> emit $ FileEvent FileAdded file
      FSNotify.Modified file _ _ -> emit $ FileEvent FileModified file
      FSNotify.ModifiedAttributes file _ _ -> emit $ FileEvent FileModified file
      FSNotify.Removed file _ _ -> emit $ FileEvent FileRemoved file
      FSNotify.WatchedDirectoryRemoved _file _ _ -> pass
      FSNotify.CloseWrite file _ _ -> emit $ FileEvent FileModified file
      FSNotify.Unknown file _ _ _ -> emit $ FileEvent FileModified file

    emit :: EventQueue.Event -> IO ()
    emit = emitEvent queue

    isInteresting :: FSNotify.Event -> Bool
    isInteresting = (&&) <$> isFile <*> not . isBoring . FSNotify.eventPath

    isFile :: FSNotify.Event -> Bool
    isFile = FSNotify.eventIsDirectory >>> (== FSNotify.IsFile)

data Mode = Lenient | Strict

run :: [String] -> IO ()
run args = do
  runArgs@RunArgs{dir, lastOutput, queue} <- defaultRunArgs
  HTTP.withServer dir (readMVar lastOutput) $ do
    watchFiles dir queue $ do
      mode <- newIORef Lenient
      Input.watch stdin (dispatch mode queue) (emitEvent queue Done)
      runWith runArgs {args}
  where
    dispatch :: IORef Mode -> EventQueue -> Char -> IO ()
    dispatch mode queue = \ case
      '\n' -> emitEvent queue TriggerAll
      'w' -> do
        modifyIORef mode toggle
        readIORef mode >>= \ case
          Lenient -> emitEvent queue (RestartWith ["-Wdefault"])
          Strict -> emitEvent queue (RestartWith ["-Wall"])
      'q' -> emitEvent queue Done
      _ -> pass

    toggle :: Mode -> Mode
    toggle = \ case
      Lenient -> Strict
      Strict -> Lenient

defaultRunArgs :: IO RunArgs
defaultRunArgs = do
  queue <- newQueue
  lastOutput <- newMVar (Trigger.Success, "", [])
  return RunArgs {
    ignoreConfig = False
  , dir = ""
  , args = []
  , lastOutput = lastOutput
  , queue = queue
  , sessionConfig = defaultSessionConfig
  , withSession = Session.withSession
  }

data RunArgs = RunArgs {
  ignoreConfig :: Bool
, dir :: FilePath
, args :: [String]
, lastOutput :: MVar (Result, String, [Diagnostic])
, queue :: EventQueue
, sessionConfig  :: Session.Config
, withSession :: forall r. Session.Config -> [String] -> (Session.Session -> IO r) -> IO r
}

runWith :: RunArgs -> IO ()
runWith RunArgs {..} = do
  config <- case ignoreConfig of
    False -> loadConfig
    True -> return defaultConfig
  cleanup <- newIORef pass
  let
    runCleanupAction :: IO ()
    runCleanupAction = join $ atomicModifyIORef' cleanup $ (,) pass

    addCleanupAction :: IO () -> IO ()
    addCleanupAction cleanupAction = atomicModifyIORef' cleanup $ \ action -> (action >> cleanupAction, ())

    saveOutput :: IO (Trigger.Result, String, [Diagnostic]) -> IO ()
    saveOutput action = do
      runCleanupAction
      result <- modifyMVar lastOutput $ \ _ -> (id &&& id) <$> action
      case result of
        (HookFailed, _output, _diagnostics) -> pass
        (Failure, output, _diagnostics) -> config.senseiHooksOnFailure >>= \ case
          HookSuccess -> pager output >>= addCleanupAction
          HookFailure message -> hPutStrLn stderr message
        (Success, _output, _diagnostics) -> config.senseiHooksOnSuccess >>= \ case
          HookSuccess -> pass
          HookFailure message -> hPutStrLn stderr message

    hooks :: Hooks
    hooks = Hooks {
      beforeReload = config.senseiHooksBeforeReload
    , afterReload = config.senseiHooksAfterReload
    }

    go :: [String] -> IO ()
    go extraArgs = do
      status <- withSession sessionConfig (extraArgs <> args) $ \ session -> do
        let
          printExtraArgs :: IO ()
          printExtraArgs = forM_ extraArgs $ \ arg -> do
            sessionConfig.configEcho . encodeUtf8 . withColor Red $ arg <> "\n"

          triggerAction = saveOutput (trigger session hooks <* printExtraArgs)
          triggerAllAction = saveOutput (triggerAll session hooks <* printExtraArgs)

        triggerAction
        processQueue runCleanupAction (sessionConfig.configEcho . encodeUtf8) dir queue triggerAllAction triggerAction
      case status of
        Restart mExtraArgs -> go (fromMaybe extraArgs mExtraArgs)
        Terminate -> return ()
  go []

runWeb :: [String] -> IO ()
runWeb args = do
  Session.withSession defaultSessionConfig args $ \session -> do
    _ <- trigger session defaultHooks
    lock <- newMVar ()
    HTTP.withServer "" (withMVar lock $ \() -> trigger session defaultHooks) $ do
      waitForever

defaultSessionConfig :: Session.Config
defaultSessionConfig = Session.Config {
  configIgnoreDotGhci = False
, configWorkingDirectory = Nothing
, configEcho = \ string -> ByteString.putStr string >> hFlush stdout
}
