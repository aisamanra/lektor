{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lektor
  ( Feed(..)
  , Entry(..)
  , mkFeed
  , mkEntry
  , writeFeed
  , writeEntry
  ) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (isDigit)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HostName (getHostName)
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import System.Posix.Process (getProcessID)

mkdirP :: FilePath -> IO ()
mkdirP = createDirectoryIfMissing True

hash :: String -> String
hash = showDigest . sha1 . pack

writeFileMb :: FilePath -> Maybe String -> IO ()
writeFileMb path = maybe (return ()) (writeFile path)

data Feed = Feed
  { feedId     :: String
  , feedName   :: String
  , feedDescr  :: Maybe String
  , feedLang   :: Maybe String
  , feedImage  :: Maybe String
  , feedCopy   :: Maybe String
  , feedAuthor :: Maybe String
  } deriving (Eq, Show)

data Entry = Entry
  { entryId      :: String
  , entryTitle   :: String
  , entryContent :: String
  , entryAuthor  :: Maybe String
  , entryPubdate :: Maybe String
  , entryType    :: Maybe String
  } deriving (Eq, Show)

mkFeed :: String -> String -> Feed
mkFeed feedId feedName =
  Feed feedId feedName Nothing Nothing Nothing Nothing Nothing

mkEntry :: String -> String -> String -> Entry
mkEntry entryId entryTitle entryContent =
  Entry entryId entryTitle entryContent Nothing Nothing Nothing

writeFeed :: Feed -> IO ()
writeFeed Feed { .. } = do
  let dir = "src" </> hash feedId
  mkdirP dir
  writeFile   (dir </> "id")          feedId
  writeFile   (dir </> "name")        feedName
  writeFileMb (dir </> "description") feedDescr
  writeFileMb (dir </> "language")    feedLang
  writeFileMb (dir </> "image")       feedImage
  writeFileMb (dir </> "copyright")   feedCopy
  writeFileMb (dir </> "author")      feedAuthor

writeEntry :: Entry -> Feed -> IO ()
writeEntry (Entry { .. }) (Feed { feedId = feedId }) = do
  let feedHash = hash feedId
  uniq <- mkUniq
  let dir = "tmp" </> feedHash </> uniq
  mkdirP dir
  mkdirP ("new" </> hash feedId)
  writeFile   (dir </> "id")      entryId
  writeFile   (dir </> "title")   entryTitle
  writeFile   (dir </> "content") entryContent
  writeFileMb (dir </> "author")  entryAuthor
  writeFileMb (dir </> "pubdate") entryPubdate
  writeFileMb (dir </> "type")    entryType
  createSymbolicLink (dir </> "feed") ("src" </> feedHash)
  renameDirectory dir ("new" </> feedHash </> uniq)

mkUniq :: IO String
mkUniq = do
  (t :: Integer, r') <- properFraction `fmap` getPOSIXTime
  let r = filter isDigit (show r')
  let m = ""
  p <- getProcessID
  h <- getHostName
  let uniq = "P" <> show p <> "R" <> r <> "M" <> m
  return (show t <> "." <> uniq <> "." <> h)
