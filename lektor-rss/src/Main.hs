{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Atom
import qualified Lektor as L

import           Control.Error (ExceptT(..), headErr, fmapLT, runScript, (??))
import           Control.Monad.Trans.Class (lift)
import           Network.HTTP (simpleHTTP, getRequest, rspBody)
import           System.Environment (getArgs)
import           Text.Atom.Feed.Import (elementFeed)
import           Text.XML.Light.Input (parseXMLDoc)

usage :: String
usage = "Usage: lektor-rss [feed url]"

main :: IO ()
main = runScript $ do
  -- check the args for the url
  url  <- ExceptT (fmap (headErr usage) getArgs)
  -- GET the resource
  resp <- fmapLT show (ExceptT (simpleHTTP (getRequest url)))
  -- parse the XML
  xml  <- parseXMLDoc (rspBody resp) ?? "Unable to parse XML document"
  -- decode the Atom format
  atom <- elementFeed xml ?? "XML document not an Atom feed"
  -- write the Lektor dir
  lift (L.writeAll (atomToLektor atom))
