{-# LANGUAGE OverloadedStrings #-}
module Model.Parse where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import qualified Data.Text as T

data MDElement = Elem T.Text
               | Link T.Text
                 deriving (Show, Eq)

data MDLine = Line [MDElement]
            | QuotedLine T.Text
              deriving (Show, Eq)

parseMarkdown :: String -> Either String [MDLine]
parseMarkdown s =
    case feed (parse mdEx $ T.pack s) T.empty of
      Done "" md -> Right md
      _ -> Left "can't parse"

mdEx = choice [preLine, normalLine] `sepBy` eol

preLine = do
  q <- try $ choice [string "\t", string "    "]
  l <- A.takeWhile isLineChar
  return $ QuotedLine $ T.concat [q, l]
    where isLineChar = (`notElem` "\r\n")

normalLine = do
  l <- A.many $ choice [wikiLink, lineChar]
  return $ Line $ reverse $ foldl f [] l
    where f (Elem x : xs) (Elem y) = (Elem $ T.append x y) : xs
          f xs y = y : xs

          lineChar = do
            c <- satisfy (`notElem` "\r\n")
            return $ Elem $ T.pack [c]

wikiLink = do
  name <- string "[[" *> A.takeWhile wikiNameChar <* string "]]"
  return $ Link name
    where wikiNameChar = inClass "a-zA-Z0-9"

eol = choice [try (string "\n\r"),
              try (string "\r\n"),
              string "\n",
              string "\r"]
