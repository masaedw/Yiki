module Main where
import Text.Pandoc
import Prelude hiding (getContents, putStrLn)
import System.IO.UTF8


markdownToHtml :: String -> String
markdownToHtml =
  (writeHtmlString defaultWriterOptions {writerReferenceLinks = True}) .
  readMarkdown defaultParserState

main = getContents >>= putStrLn . markdownToHtml
