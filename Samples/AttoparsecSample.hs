{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec
import Data.Attoparsec.Char8 hiding (satisfy)
import qualified Data.ByteString as B

mdEx = choice [preLine, normalLine] `sepBy` eol

preLine = do
  try $ choice [string "\t", string "    "]
  normalLine

normalLine = satisfy anyChar
    where anyChar = (`B.notElem` "\r\n")

eol = choice [try (string "\n\r"),
              try (string "\r\n"),
              string "\n",
              string "\r"]
      <?> "end of line"
