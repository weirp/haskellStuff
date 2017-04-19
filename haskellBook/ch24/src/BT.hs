module BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)
import qualified Data.ByteString.Char8 as B

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: (Show a) => Parsec String () a -> String -> IO ()
parsecP = parseTest


attoP :: Show a => A.Parser a -> String -> IO ()
attoP p i = print $ parseOnly p (B.pack  i)

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse =
  try (char '1' >> char '2')
  <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot =
  (try (char '1' >> char '2')
  <?> "Tried 12")
  <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
  -- trifecta
  trifP nobackParse "13"
  trifP tryParse "13"
  -- parsec
  parsecP nobackParse "13"
  parsecP tryParse "13"
  -- attoparsec
  attoP nobackParse "13"
  attoP tryParse "13"
