{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Lektor as L

import qualified Atom as A

import Control.Monad (zipWithM_)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HostName
import Network.HTTP
import System.Directory
import System.Environment
-- import System.Exit (die)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import Text.Atom.Feed.Import (elementFeed)
import Text.Atom.Feed
import Text.XML.Light.Input (parseXMLDoc)

die = error

usage :: String
usage = "Usage: lektor-rss [feed url]"

lektorSetup :: IO ()
lektorSetup = do
  dir <- getEnv "LEKTORDIR"
  setCurrentDirectory dir
  mapM_ (createDirectoryIfMissing True)
    [ "src", "tmp", "new", "cur" ]

main :: IO ()
main = do
  lektorSetup
  getArgs >>= \case
    []      -> putStrLn usage
    (url:_) -> do
      simpleHTTP (getRequest url) >>= \case
        Left err -> die "Unable to fetch document"
        Right r  -> makeEntries url (rspBody r)

makeEntries :: String -> String -> IO ()
makeEntries url s = case parseXMLDoc s of
  Nothing -> die "Unale to parse XML document"
  Just xml -> case elementFeed xml of
    Just atom -> buildLektorDir url atom
    Nothing   -> die "XML document not an Atom feed"

contentAsString :: TextContent -> String
contentAsString (TextString s) = s
contentAsString _ = error "..."

buildLektorDir :: String -> Feed -> IO ()
buildLektorDir url feed = do
  let hash = showDigest (sha1 (pack url))
  mapM_ (createDirectoryIfMissing True)
    [ "src" </> hash
    , "tmp" </> hash
    , "new" </> hash
    , "cur" </> hash
    ]
  writeFile ("src" </> hash </> "name")
    (contentAsString (feedTitle feed))
  writeFile ("src" </> hash </> "id") url
  zipWithM_ (buildLektorEntry hash) [0..] (reverse (feedEntries feed))

buildLektorEntry :: String -> Int -> Entry -> IO ()
buildLektorEntry hash n (Entry { .. }) = do
  t <- fmap (floor . realToFrac) getPOSIXTime
  p <- getProcessID
  h <- getHostName
  let dirId  = show t <> ".P" <> show p <> "Q" <> show n <> "." <> h
  let tmpDir = "tmp" </> hash </> dirId
  createDirectoryIfMissing True tmpDir
  writeFile (tmpDir </> "title") (contentAsString entryTitle)
  writeFile (tmpDir </> "id") entryId
  writeFile (tmpDir </> "content") $ case entryContent of
    Just (TextContent s) -> s
    Just (HTMLContent s) -> s
    _                    -> "[unsupported content]"
  writeFile (tmpDir </> "type") $ case entryContent of
    Just (HTMLContent s) -> "text/html"
    _                    -> "text/plain"
  renameDirectory tmpDir ("new" </> hash </> dirId)
