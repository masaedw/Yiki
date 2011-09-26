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
-- Foundation
------------------------------------------------------------

data Yiki = Yiki ConnectionPool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
YikiPage
    name String
    body String
    updated UTCTime default=CURRENT_TIMESTAMP
    created UTCTime default=CURRENT_TIMESTAMP
    UniqueName name
|]


mkYesod "Yiki" [parseRoutes|
/ HomeR GET
/pages/#Text PageR GET
/pages/#Text/edit EditR GET POST
/pages/#Text/delete DeleteR POST
/index IndexR GET
|]

------------------------------------------------------------
-- Design
------------------------------------------------------------

-- toWidgetWithとモデル定義の型がここからだと見えない。。
layoutWithSidebar content = do
  yikiSidebar <- runDB $ getPage "sidebar"
  urlRender <- getUrlRender
  let yikiSidebar' = sidebar urlRender (snd <$> yikiSidebar)
  titleId <- newIdent
  mainId <- newIdent
  sidebarId <- newIdent
  contentId <- newIdent
  defaultLayout' $ do
    addCassius [cassius|
html
    height: 100%;
body
    height: 100%;
    background-color: #EBE1AD;
    margin: 0;
    color: #423B0B;
header
    background-color: #666149;
    color: #FFF;
##{titleId}
    width: 900px;
    height: 80px;
    position: relative;
    margin: 0 auto;
    padding-top: 25px;
    color: #fff;
##{mainId}
    width: 900px;
    position: relative;
    margin: 0 auto;
##{sidebarId}
    width: 310px;
    float: right;
    padding-bottom: 40px;
##{contentId}
    width: 500px;
    float: left;
    text-shadow: 0 1px 0 #fff;
    padding-bottom: 40px;
footer
    width: 900px;
    position: relative;
    margin: 0 auto;
    clear: both;
    padding: 30px 0;
footer p
    font-size: 11px;
    line-height: 18px;
    text-shadow: 0 1px 0 #fff;
h1, h2, h3
    color: #221F66;
|]
    addWidget [whamlet|
<header>
  <h1 ##{titleId}>Yiki: a simple wiki
<div ##{mainId}>
  <div ##{contentId}> ^{content}
  <div ##{sidebarId}>
|]

sidebar :: (YikiRoute -> Text) -> Maybe YikiPage -> Widget
sidebar routeRender = maybe redirectWidget (`toWidgetWith` routeRender)
    where
      redirectWidget :: Widget
      redirectWidget = [whamlet| うーん　どうしませう。。 |]


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
        <footer>
          <p>
            powerd by <a href=http://www.github.com/masaedw/Yiki/>Yiki</a><br/>
            powerd by <a href=http://www.yesodweb.com/>yesod</a>
|]

------------------------------------------------------------
-- Models
------------------------------------------------------------

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
  map snd <$> selectList [] [Desc YikiPageUpdated]
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


-- display all articles

getIndexR :: Handler RepHtml
getIndexR = do
  pages <- runDB $ getAllPages
  defaultLayout [whamlet|
<h1>Index
<h2> All articles
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
  <a href=@{EditR name}>edit</a> <a href=@{IndexR}>index</a>
|]

yikiPageNameField = checkBool validateYikiPageName errorMessage textField
    where
      errorMessage :: Text
      errorMessage = pack $
                     "Unacceptable page name!" ++
                     " Available page name must be composed of" ++
                     " only alphabet and digit."


toWidgetWith :: YikiPage -> (YikiRoute -> Text) -> Widget
page `toWidgetWith` routeRender = either failure success rendered
    where
      body = yikiPageBody page
      name = pack $ yikiPageName page

      rendered :: Either String String
      rendered = markdownToHtml routeRender body

      success :: String -> Widget
      success html = [whamlet|^{toolbar name}<p>#{preEscapedString html}|]

      failure :: String -> Widget
      failure err = [whamlet|
^{toolbar name}
<p>#{err}
<pre>#{body}
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

