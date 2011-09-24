{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Attoparsec
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B

data MDElement = Elem B.ByteString
               | Link B.ByteString
                 deriving (Show, Eq)

data MDLine = Line [MDElement]
            | QuotedLine B.ByteString
              deriving (Show, Eq)

mdEx = choice [preLine, normalLine] `sepBy` eol

preLine = do
  q <- try $ choice [string "\t", string "    "]
  l <- A.takeWhile lineChar
  return $ QuotedLine $ B.concat [q, l]
    where lineChar = (`B.notElem` "\r\n")

normalLine = do
  l <- A.takeWhile lineChar
  return $ Line [Elem l]
    where lineChar = (`B.notElem` "\r\n")

wikiLink = do
  name <- string "[[" *> A.takeWhile wikiNameChar <* string "]]"
  return $ Link name
    where wikiNameChar = inClass "a-zA-Z0-9"

eol = choice [try (string "\n\r"),
              try (string "\r\n"),
              string "\n",
              string "\r"]
      <?> "end of line"

main = do
  f <- B.readFile "sample.md"
  let p = parse mdEx f
  print $ feed p B.empty
