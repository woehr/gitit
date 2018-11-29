module Network.Gitit.Plugins.Signature (plugin) where

-- This plugin replaces $SIG$ with the username and timestamp
-- of the last edit, prior to saving the page in the repository.

import Network.Gitit.Interface
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)

plugin :: Plugin
plugin = PreCommitTransform replacedate

replacedate :: String -> PluginM String
replacedate [] = return ""
replacedate ('$':'S':'I':'G':'$':xs) = do
  utcTime <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  let datetime = utcToLocalTime tz utcTime
  mbuser <- askUser
  let username = case mbuser of
                   Nothing  -> "???"
                   Just u   -> uUsername u
  let sig = concat ["-- ", username, " (", formatTime defaultTimeLocale "%c" datetime, ")"]
  fmap (sig ++ ) $ replacedate xs
replacedate (x:xs) = fmap (x : ) $ replacedate xs
