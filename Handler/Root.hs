{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Handler.Root where

import Model.Accessor
import Foundation


import Control.Applicative
import Data.Text (Text, pack, unpack)
import Text.Blaze
import qualified Data.Text as T


-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = getPageR "home"

-- read

getPageR :: Text -> Handler RepHtml
getPageR pageName = do
  page <- runDB $ getPage pageName
  case page of
    Nothing -> do redirect RedirectTemporary $ EditR pageName
    Just (id,page) -> do
      render <- getUrlRender
      let body = page `toWidgetWith` render
      sidebarLayout [whamlet|^{toolbar pageName}^{body}|]

-- create & update

data YikiPageEdit = YikiPageEdit
  { peBody :: Textarea
  }

toPageEdit :: YikiPage -> YikiPageEdit
toPageEdit yp =
    YikiPageEdit $ Textarea $ yikiPageBody yp

yikiPageEditForm :: Maybe YikiPageEdit -> Html -> Form Yiki Yiki (FormResult YikiPageEdit, Widget)
yikiPageEditForm ype = renderDivs $ YikiPageEdit <$> areq textareaField "" (peBody <$> ype)

getEditR :: Text -> Handler RepHtml
getEditR pageName = do
  -- result :: Maybe (YikiPageId, YikiPage)
  result <- runDB $ getPage pageName
  let edit = case result of
               Nothing -> YikiPageEdit $ Textarea ""
               Just (_,page) -> toPageEdit page
  ((_, widget), enctype) <- generateFormPost $ yikiPageEditForm $ Just edit
  sidebarLayout [whamlet|
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
          let body = T.filter (`notElem` "\r") $ unTextarea $ peBody ype
          runDB $ createOrUpdatePageBody pageName body
          redirect RedirectTemporary $ PageR pageName
        _ -> sidebarLayout [whamlet|
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
  sidebarLayout [whamlet|
<h1>Index
<h2> All articles
$if null pages
    No Articles
$else
    <ul>
        $forall page <- pages
            <li><a href=@{PageR $ yikiPageName page}>#{yikiPageName page}</a> #{show $ yikiPageCreated page}
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
      name = yikiPageName page

      rendered :: Either String String
      rendered = markdownToHtml routeRender $ unpack body

      success :: String -> Widget
      success html = [whamlet|<p>#{preEscapedString html}|]

      failure :: String -> Widget
      failure err = [whamlet|<p>#{err}</p><pre>#{body}</pre>|]



------------------------------------------------------------
-- Design
------------------------------------------------------------

sidebarLayout :: Widget -> Handler RepHtml
sidebarLayout content = do
  yikiSidebar <- runDB $ getPage "sidebar"
  urlRender <- getUrlRender
  let sidebar = toSidebarWith (snd <$> yikiSidebar) urlRender
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
    width: 240px;
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
textarea
    resize: none;
    font-size: medium;
    width: 125%;
    height: 500px;
    border: 3px solid #cccccc;
    padding: 5px;
    font-family: Tahoma, sans-serif;
    background-position: bottom right;
    background-repeat: no-repeat;
|]
    addWidget [whamlet|
<header>
  <h1 ##{titleId}>Yiki: a simple wiki
<div ##{mainId}>
  <div ##{contentId}> ^{content}
  <div ##{sidebarId}> ^{sidebar}
|]

toSidebarWith :: Maybe YikiPage -> (YikiRoute -> Text) -> Widget
page `toSidebarWith` routeRender = maybe redirectWidget (`toWidgetWith` routeRender) page
    where
      redirectWidget = [whamlet| hoge |]

defaultLayout' :: Widget -> Handler RepHtml
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

