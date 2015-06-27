{-# LANGUAGE RecordWildCards #-}

module Atom (atomToLektor) where

import           Data.Monoid ((<>))
import           Text.Atom.Feed
import           Text.XML.Light.Output

import qualified Lektor as L

atomToLektor :: Feed -> (L.Feed, [L.Entry])
atomToLektor Feed { .. } = (lfeed, lentries)
  where lentries = map toEntry feedEntries
        lfeed = L.Feed
          { L.feedId     = feedId
          , L.feedName   = toString feedTitle
          , L.feedDescr  = fmap toString feedSubtitle
          , L.feedLang   = Nothing
          , L.feedImage  = feedLogo
          , L.feedCopy   = fmap toString feedRights
          , L.feedAuthor = toPeople feedAuthors
          }

toEntry :: Entry -> L.Entry
toEntry Entry { .. } = L.Entry
  { L.entryId      = entryId
  , L.entryTitle   = toString entryTitle
  , L.entryContent = maybe "" entryToString entryContent
  , L.entryAuthor  = toPeople entryAuthors
  , L.entryPubdate = undefined
  , L.entryType    = undefined
  }

toString :: TextContent -> String
toString (TextString s)  = s
toString (HTMLString s)  = s
toString (XHTMLString e) = showElement e

entryToString :: EntryContent -> String
entryToString (TextContent s)       = s
entryToString (HTMLContent s)       = s
entryToString (XHTMLContent e)      = showElement e
entryToString (MixedContent _ _)    = "[unimplemented]"
entryToString (ExternalContent _ _) = "[unimplemented]"

toPeople :: [Person] -> Maybe String
toPeople [] = Nothing
toPeople xs = Just (unlines (map toPerson xs))

toPerson :: Person -> String
toPerson p = case personEmail p of
  Just e  -> personName p <> " <" <> e <> ">"
  Nothing -> personName p
