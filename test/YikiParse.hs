{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module YikiParserTest where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad
import Data.Char (isPrint)
import Data.List (intersperse)
import Data.Text (Text, pack, unpack, append)
import Test.QuickCheck
import Test.QuickCheck.All
import Text.Regex.PCRE

import Yiki.Parse

alnum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
linkText = pack <$> (listOf $ arbitrary `suchThat` (`elem` alnum))
elemText = pack <$> arbitrary `suchThat` \x ->
             not $ (x =~ "(\\A\t|    )|\\[\\[[a-zA-Z0-9]*\\]\\]|[\n\r]" || x == [])

instance Arbitrary MDElement where
    arbitrary =
        oneof [ Elem <$> elemText
              , Link <$> linkText ]

instance Arbitrary MDLine where
    arbitrary =
        frequency [ (10, Line <$> normalize <$> arbitrary)
                  , (1, do
                       marker <- oneof [ pure "    ", pure "\t" ]
                       body <- arbitrary `suchThat` (\x -> not $ (x == "" || all isPrint x || x =~ "[\n\r]"))
                       let line = marker ++ body
                       QuotedLine <$> pack <$> pure line) ]
            where normalize = foldl f []
                  f (Elem x : xs) (Elem y) = (Elem $ append x y) : xs
                  f xs y = y : xs

buildString = concat . intersperse "\n" . map lineString
    where
      lineString (QuotedLine l) = unpack l
      lineString (Line els) = concatMap elementString els

      elementString (Elem l) = unpack l
      elementString (Link l) = "[[" ++ unpack l ++ "]]"

prop_parserShouldAlwaysSuccess s =
    case parseMarkdown s of
      Right _ -> True
      Left _ -> False

prop_parserRandomData [] = True
prop_parserRandomData lines =
    case parseMarkdown $ buildString lines of
      Right result -> result == lines
      Left _ -> False

main = $quickCheckAll
