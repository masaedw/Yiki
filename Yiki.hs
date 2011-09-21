{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Yiki where
import Control.Monad.IO.Class
import Data.Text
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Page
    name String
    body String
    created UTCTime default='now'
|]


data Yiki = Yiki ConnectionPool

mkYesod "Yiki" [parseRoutes|
/ HomeR GET
/page/#Text PageR GET POST
/page/#Text/edit EditR GET
|]

instance Yesod Yiki where
    approot _ = ""

instance YesodPersist Yiki where
    type YesodPersistBackend Yiki = SqlPersist

    runDB action = liftIOHandler $ do
      Yiki pool <- getYesod
      runSqlPool action pool

defaultPage = [whamlet|
<h1>Welcome to Yiki

<p>This is the start page. You can Edit this page from <a href="@{EditR "home"}">here</a>.
|]

getHomeR = defaultLayout defaultPage

getPageR :: Text -> Handler RepHtml
getPageR pageId = undefined

postPageR :: Text -> Handler RepHtml
postPageR pageId = undefined

getEditR :: Text -> Handler RepHtml
getEditR pageId = undefined

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = withSqlitePool "yiki.sqlite" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    warpDebug 3000 $ Yiki pool

