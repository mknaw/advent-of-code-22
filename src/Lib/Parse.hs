module Lib.Parse
  ( Parser,
    parseInput,
    parseInt,
    parseIntLines,
  )
where

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parseInput :: Parser a -> Text -> a
parseInput parser input = 
  case runParser parser "" input of
    Left err -> error $ show err
    Right x -> x

parseInt :: Parser Int
parseInt = read <$> some digitChar

parseIntLines :: Parser [Int]
parseIntLines = parseInt `sepEndBy` eol
