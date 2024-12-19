{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module XMonad where

-- import Control.Exception (IOException, try)
import System.Process
import System.Posix.Process (getProcessID)
-- import Data.Maybe (listToMaybe)
import Data.Functor
import System.Exit
import Data.List

import Imports

newtype PID = PID String
newtype XWindowId = XWindowId String
newtype Tag = Tag String

instance IsString Tag where
  fromString = Tag

-- Get the parent PID of a process
getParentPid :: PID -> IO (Maybe PID)
getParentPid (PID pid) = readProcessWithExitCode "ps" ["-o", "ppid=", "-p", pid] "" <&> \ case
  (_, result, _) -> case strip result of
    "" -> Nothing
    ppid -> Just (PID ppid)

getWindowIdForPid :: PID -> IO (Maybe XWindowId)
getWindowIdForPid (PID pid) = do
  windowIds <- filter (isPrefixOf "0x") . words <$> readProcess "xprop" ["-root", "_NET_CLIENT_LIST"] ""
  findWindow windowIds
  where
    findWindow :: [String] -> IO (Maybe XWindowId)
    findWindow = \ case
      [] -> return Nothing
      wid : rest -> do
        (_, wmPidOutput, _) <- readProcessWithExitCode "xprop" ["-id", wid, "_NET_WM_PID"] ""
        case reverse (words wmPidOutput) of
          wmPid : _ | wmPid == pid -> return (Just $ XWindowId wid)
          _ -> findWindow rest

findAncestorWindowId :: PID -> IO (Maybe XWindowId)
findAncestorWindowId pid = do
  windowId <- getWindowIdForPid pid
  case windowId of
    Just wid -> return (Just wid)
    Nothing -> do
      parentPid <- getParentPid pid
      case parentPid of
        Just ppid -> findAncestorWindowId ppid
        Nothing -> return Nothing

addTag :: Tag -> XWindowId -> IO ()
addTag (Tag name) (XWindowId wid) = do
  result <- readProcess "xprop" ["-id", wid, "_XMONAD_TAGS"] ""
  
  let 
    tags :: String
    tags = if "not found" `elem` words result then "" else extractTags result
    
    newTags :: String
    newTags = if null tags then name else tags <> " " <> name
  
  callProcess "xprop" ["-id", wid, "-f", "_XMONAD_TAGS", "8s", "-set", "_XMONAD_TAGS", newTags]
  where
    extractTags :: String -> String
    extractTags = unwords . drop 1 . words . last . lines

tagSelfWith :: Tag -> IO ()
tagSelfWith name = do
  pid <- PID . show <$> getProcessID
  result <- findAncestorWindowId pid
  case result of
    Just wid -> addTag name wid
    Nothing -> exitFailure
