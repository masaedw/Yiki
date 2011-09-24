{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Yiki where
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Text hiding (null, map, filter)
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Text.Blaze
import Text.Pandoc
import Text.Cassius
import Yesod
import Yesod.Form.Jquery


------------------------------------------------------------
-- Design
------------------------------------------------------------

layoutWithSidebar content = do
  sidebarId <- newIdent
  contentId <- newIdent
  defaultLayout' $ do
    addCassius [cassius|
html
    height: 100%;
body
    height: 100%;
##{sidebarId}
    color: red;
    font-size: bold;
    width: 180px;
    height: 100%;
    float: left;
    margin: 1px;
    padding: 0;
    background: #FFFAF0;
##{contentId}
   width: 500px;
   height: 100%
   margin: 0;
   padding: 0;
   float: left
|]
    addWidget [whamlet|
<div ##{sidebarId}> ^{sidebar}
<div ##{contentId}> ^{content}
|]

-- sidebar :: Monad m => GGWidget master m ()
sidebar = [whamlet|
   What do you want to put here?
   -- TODO
   -- サイドバーのコンテンツを決める入れる
|]

-- Professional Programmer's Work... (See src of Yesod.Core)
defaultLayout' :: (Yesod a) => GWidget sub a () -> GHandler sub a RepHtml
defaultLayout' w = do
  p <- widgetToPageContent w
  mmsg <- getMessage
  hamletToRepHtml [hamlet|
!!!

<html>
    <head>
        <title>#{pageTitle p}
        ^{pageHead p}
    <body>
        $maybe msg <- mmsg
            <p .message>#{msg}
        ^{pageBody p}
|]

------------------------------------------------------------
-- Models
------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
YikiPage
    name String
    body String
    updated UTCTime default="datetime('now')"
    created UTCTime default="datetime('now')"
|]

getPage name = do
  selectFirst [YikiPageName ==. name] []

updatePageBody name body = do
  now <- liftIO getCurrentTime
  updateWhere [YikiPageName ==. name]
              [YikiPageBody =. body, YikiPageUpdated =. now]

getPages 0 = do
  map snd <$> selectList [] []
getPages n = do
  map snd <$> selectList [] [LimitTo n]

getAllPages = getPages 0

numOfPages = do
  Yesod.count ([] :: [Filter YikiPage])

insertDefaultDataIfNecessary = do
  numOfPages <- numOfPages
  when (numOfPages == 0) $ do
    body <- liftIO $ readFile "Samples/sample.md"
    now <- liftIO getCurrentTime
    insert $ YikiPage "home" body now now
    return ()


markdownToHtml :: String -> String
markdownToHtml =
  (writeHtmlString defaultWriterOptions {writerReferenceLinks = True}) .
  readMarkdown defaultParserState

------------------------------------------------------------
-- Applicaiton
------------------------------------------------------------

data Yiki = Yiki ConnectionPool

instance Yesod Yiki where
    approot _ = ""
    defaultLayout = layoutWithSidebar

instance YesodPersist Yiki where
    type YesodPersistBackend Yiki = SqlPersist

    runDB action = liftIOHandler $ do
      Yiki pool <- getYesod
      runSqlPool action pool

instance RenderMessage Yiki FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery Yiki

mkYesod "Yiki" [parseRoutes|
/ HomeR GET
/new NewR GET POST
/pages/#Text PageR GET
/pages/#Text/edit EditR GET POST
/pages/#Text/delete DeleteR POST
/index IndexR GET
|]

openConnectionCount :: Int
openConnectionCount = 10

------------------------------------------------------------
-- Handlers
------------------------------------------------------------

---- Home

getHomeR = getPageR "home"


---- YikiPages

-- create

getNewR :: Handler RepHtml
getNewR = undefined

postNewR :: Handler RepHtml
postNewR = undefined


-- read

getPageR :: Text -> Handler RepHtml
getPageR pageName = do
  page <- runDB $ getPage name
  case page of
    Nothing -> defaultLayout [whamlet|<p>no such page: #{name}|]
    Just (id,page) -> do
      liftIO $ print $ yikiPageBody page
      let content = preEscapedString $ markdownToHtml $ yikiPageBody page
      defaultLayout [whamlet|^{toolbar pageName}<p>#{content}|]
  where name = unpack pageName


-- update

data YikiPageEdit = YikiPageEdit
  { peName :: Text
  , peBody :: Textarea
  }

toPageEdit :: YikiPage -> YikiPageEdit
toPageEdit yp =
    YikiPageEdit (pack $ yikiPageName yp) (Textarea $ pack $ yikiPageBody yp)

yikiPageForm :: Maybe YikiPageEdit -> Html -> Form Yiki Yiki (FormResult YikiPageEdit, Widget)
yikiPageForm ype = renderDivs $ YikiPageEdit
  <$> areq textField "Name" (peName <$> ype)
  <*> areq textareaField "Body" (peBody <$> ype)

getEditR :: Text -> Handler RepHtml
getEditR pageName = do
  -- result :: Maybe (YikiPageId, YikiPage)
  result <- runDB $ getPage $ unpack pageName
  case result of
    Nothing -> defaultLayout [whamlet|<p>no such page: #{unpack pageName}|]
    Just (_,page) -> do
       ((_, widget), enctype) <- generateFormPost $ yikiPageForm $ Just $ toPageEdit $ page
       defaultLayout [whamlet|
<form method=post action=@{EditR pageName} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
  <a href=@{PageR pageName}>cancel
<p>
|]


postEditR :: Text -> Handler RepHtml
postEditR pageName = do
    ((result, widget), enctype) <- runFormPost $ yikiPageForm Nothing
    case result of
        FormSuccess ype -> do
          let body = unpack $ T.filter (`notElem` "\r") $ unTextarea $ peBody ype
          let name = unpack $ peName ype
          runDB $ updatePageBody name body
          redirect RedirectTemporary $ PageR pageName
        _ -> defaultLayout [whamlet|
<p>Invalid input, let's try again.
<form method=post action=@{EditR pageName} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
|]

-- delete

postDeleteR :: Text -> Handler RepHtml
postDeleteR = undefined


-- display all the articles

getIndexR :: Handler RepHtml
getIndexR = do
  pages <- runDB $ getAllPages
  defaultLayout [whamlet|
<h1>Index
<h2> All the articles
$if null pages
    No Articles
$else
    <ul>
        $forall page <- pages
            <li><a href=@{PageR $ pack $ yikiPageName page}>#{yikiPageName page}</a> #{show $ yikiPageCreated page}
|]

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

toolbar name = [whamlet|
<div .toolbar>
  <a href=@{EditR name}>edit
|]

------------------------------------------------------------
-- Driver
------------------------------------------------------------

driver f = withSqlitePool "yiki.sqlite" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    runSqlPool insertDefaultDataIfNecessary pool
    f $ Yiki pool

main :: IO ()
main = driver $ warpDebug 3000

withYiki f = driver $ \app -> (toWaiApp $ app) >>= f

