{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Yiki where
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Text (Text, pack, unpack)
import Data.Time
import Data.List (intersperse)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Text.Blaze
import Text.Cassius
import Text.Pandoc
import Text.Printf (printf)
import Yesod
import Yesod.Form.Jquery
import qualified Data.Text as T

import Yiki.Parse


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
    UniqueName name
|]

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

getPages 0 = do
  map snd <$> selectList [] []
getPages n = do
  map snd <$> selectList [] [LimitTo n]

getAllPages = getPages 0

numOfPages = do
  Yesod.count ([] :: [Filter YikiPage])

validateYikiPageName :: Text -> Bool
validateYikiPageName = all isAlphaNum . unpack

insertDefaultDataIfNecessary = do
  numOfPages <- numOfPages
  when (numOfPages == 0) $ do
    body <- liftIO $ readFile "Samples/sample.md"
    now <- liftIO getCurrentTime
    insert $ YikiPage "home" body now now
    return ()

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
/pages/#Text PageR GET
/pages/#Text/edit EditR GET POST
/pages/#Text/delete DeleteR POST
/index IndexR GET
|]

openConnectionCount :: Int
openConnectionCount = 10



-- ほんとうはこれはモデルのところに置きたい
markdownToHtml :: (YikiRoute -> Text) -> String -> Either String String
markdownToHtml render s =
    case parseMarkdown s of
      Right md ->
          Right $
          (writeHtmlString defaultWriterOptions {writerReferenceLinks = True}) $
          readMarkdown defaultParserState $
          renderMD md
      Left msg -> Left msg
    where
      renderMD :: [MDLine] -> String
      renderMD lines = concat $ intersperse "\n" $ map renderLine lines

      renderLine :: MDLine -> String
      renderLine (Line x) = concat $ map renderElem x
      renderLine (QuotedLine x) = unpack x

      renderElem :: MDElement -> String
      renderElem (Elem x) = unpack x
      renderElem (Yiki.Parse.Link x) = printf "<a href='%s'>%s</a>" url name
          where url = unpack $ render $ PageR x
                name = unpack x


------------------------------------------------------------
-- Handlers
------------------------------------------------------------

---- Home

getHomeR = getPageR "home"


---- YikiPages


-- read

getPageR :: Text -> Handler RepHtml
getPageR pageName = do
  page <- runDB $ getPage name
  case page of
    Nothing -> do redirect RedirectTemporary $ EditR pageName
    Just (id,page) -> do
      render <- getUrlRender
      let body = yikiPageBody page
      case markdownToHtml render body of
        Right html -> defaultLayout [whamlet|^{toolbar pageName}<p>#{preEscapedString html}|]
        Left err -> defaultLayout [whamlet|
^{toolbar pageName}
<p>#{err}
<pre>#{body}
|]
  where name = unpack pageName


-- create & update

data YikiPageEdit = YikiPageEdit
  { peBody :: Textarea
  }

toPageEdit :: YikiPage -> YikiPageEdit
toPageEdit yp =
    YikiPageEdit $ Textarea $ pack $ yikiPageBody yp

yikiPageEditForm :: Maybe YikiPageEdit -> Html -> Form Yiki Yiki (FormResult YikiPageEdit, Widget)
yikiPageEditForm ype = renderDivs $ YikiPageEdit
  <$> areq textareaField "" (peBody <$> ype)

getEditR :: Text -> Handler RepHtml
getEditR pageName = do
  -- result :: Maybe (YikiPageId, YikiPage)
  result <- runDB $ getPage $ unpack pageName
  let edit = case result of
               Nothing -> YikiPageEdit $ Textarea ""
               Just (_,page) -> toPageEdit page
  ((_, widget), enctype) <- generateFormPost $ yikiPageEditForm $ Just edit
  defaultLayout [whamlet|
<h1>#{unpack pageName}
<form method=post action=@{EditR pageName} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
  <a href=@{PageR pageName}>cancel
<p>
|]


postEditR :: Text -> Handler RepHtml
postEditR pageName = do
    ((result, widget), enctype) <- runFormPost $ yikiPageEditForm Nothing
    case result of
        FormSuccess ype -> do
          let body = unpack $ T.filter (`notElem` "\r") $ unTextarea $ peBody ype
          runDB $ createOrUpdatePageBody (unpack pageName) body
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

yikiPageNameField = checkBool validateYikiPageName errorMessage textField
    where
      errorMessage :: Text
      errorMessage = pack $
                     "Unacceptable page name!" ++
                     " Available page name must be composed of" ++
                     " only alphabet and digit."

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

