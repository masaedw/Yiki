{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Model.Accessor where

import Yiki.Parse
import Foundation
import Model

import Yesod

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.List (intersperse)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Data.Time (getCurrentTime, UTCTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Text.Pandoc
import Text.Printf (printf)
import Prelude hiding (readFile)

------------------------------------------------------------
-- Models
------------------------------------------------------------
getPage :: PersistBackend b m
        => Text
        -> b m (Maybe
                (Key b (YikiPageGeneric backend), YikiPageGeneric backend))
getPage name = do
  getBy $ UniqueName name

createOrUpdatePageBody name body = do
  now <- liftIO getCurrentTime
  result <- getBy $ UniqueName name
  case result of
    Just (key,_) -> update key [YikiPageBody =. body, YikiPageUpdated =. now]
    Nothing -> do
      insert $ YikiPage name body now now
      return ()

getPages :: (Functor (b m), PersistBackend b m)
         => Int -> b m [YikiPageGeneric backend]
getPages 0 = do
  map snd <$> selectList [] [Desc YikiPageUpdated]
getPages n = do
  map snd <$> selectList [] [LimitTo n]

getAllPages :: (Functor (b m), PersistBackend b m)
            => b m [YikiPageGeneric backend]
getAllPages = getPages 0

numOfPages :: PersistBackend b m => b m Int
numOfPages = do
  Yesod.count ([] :: [Filter YikiPage])
validateYikiPageName :: Text -> Bool
validateYikiPageName = T.all isAlphaNum

insertDefaultDataIfNecessary :: (PersistBackend b m) => b m ()
insertDefaultDataIfNecessary = do
  numOfPages <- numOfPages
  when (numOfPages == 0) $ do
    body <- liftIO $ readFile "Samples/sample.md"
    now <- liftIO getCurrentTime
    insert $ YikiPage "home" body now now
    return ()

markdownToHtml :: (YikiRoute -> Text) -> String -> Either String String
markdownToHtml urlRender s =
    render <$> parseMarkdown s
    where
      render :: [MDLine] -> String
      render = (writeHtmlString defaultWriterOptions {writerReferenceLinks = True}) .
               readMarkdown defaultParserState .
               mdRender

      mdRender :: [MDLine] -> String
      mdRender lines = concat $ intersperse "\n" $ map lineRender lines

      lineRender :: MDLine -> String
      lineRender (Line x) = concat $ map elemRender x
      lineRender (QuotedLine x) = unpack x

      elemRender :: MDElement -> String
      elemRender (Elem x) = unpack x
      elemRender (Yiki.Parse.Link x) = printf "<a href='%s'>%s</a>" url name
          where url = unpack $ urlRender $ PageR x
                name = unpack x