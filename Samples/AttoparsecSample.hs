{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B

data X = Line B.ByteString
       | QuotedLine B.ByteString
         deriving (Show, Eq)

mdEx = choice [preLine, normalLine] `sepBy` eol

preLine = do
  q <- try $ choice [string "\t", string "    "]
  Line l <- normalLine
  return $ QuotedLine $ B.concat [q, l]

normalLine = do
  l <- A.takeWhile lineChar
  return $ Line l
    where lineChar = (`B.notElem` "\r\n")

eol = choice [try (string "\n\r"),
              try (string "\r\n"),
              string "\n",
              string "\r"]
      <?> "end of line"

main = do
  f <- B.readFile "sample.md"
  print $ parse mdEx f -- Partial _
  -- What I expect is [Line "...", Line "...", Line "..."].
  -- Where is my mistake?
