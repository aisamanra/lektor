{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Lektor as L
import qualified Atom as A

import Network.HTTP
import System.Directory
import System.Environment
import Text.Atom.Feed.Import (elementFeed)
import Text.XML.Light.Input (parseXMLDoc)

die :: String -> a
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
        Left _  -> die "Unable to fetch document"
        Right r -> writeAsAtom (rspBody r)

writeAsAtom :: String -> IO ()
writeAsAtom s = case parseXMLDoc s of
  Nothing  -> die "Unable to parse XML document"
  Just xml -> case elementFeed xml of
    Nothing   -> die "XML document not an Atom feed"
    Just atom -> L.writeAll (A.atomToLektor atom)
