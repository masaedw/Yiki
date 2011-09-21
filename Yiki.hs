{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Yiki where
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Yesod
import Yesod.Form.Fields
import Yesod.Form.Jquery


------------------------------------------------------------
-- Models
------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
YikiPage
    name String
    body String
    created UTCTime default='now'
|]

getPage name = do
  selectFirst [YikiPageName ==. name] []

numOfPages = do
  Yesod.count ([] :: [Filter YikiPage])

insertDefaultDataIfNecessary = do
  numOfPages <- numOfPages
  when (numOfPages == 0) $ do
    body <- liftIO $ readFile "Samples/sample.md"
    now <- liftIO getCurrentTime
    insert $ YikiPage "home" body now
    return ()


------------------------------------------------------------
-- Applicaiton
------------------------------------------------------------

data Yiki = Yiki ConnectionPool

instance Yesod Yiki where
    approot _ = ""

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
|]

openConnectionCount :: Int
openConnectionCount = 10

------------------------------------------------------------
-- Handlers
------------------------------------------------------------

defaultPage = [whamlet|
<h1>Welcome to Yiki

<p>This is the start page. You can Edit this page from <a href="@{EditR "home"}">here</a>.
|]


---- Home

getHomeR = defaultLayout defaultPage


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
    Just (id,page) -> defaultLayout [whamlet|<p>#{yikiPageBody page}|]
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
<form method=post action=@{PageR pageName} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
<p>
|]


postEditR :: Text -> Handler RepHtml
postEditR pageId = undefined


-- delete

postDeleteR :: Text -> Handler RepHtml
postDeleteR = undefined


------------------------------------------------------------
-- Driver
------------------------------------------------------------

main :: IO ()
main = withSqlitePool "yiki.sqlite" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    runSqlPool insertDefaultDataIfNecessary pool
    warpDebug 3000 $ Yiki pool

