{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module WantedStrict where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ

sectionJson :: ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

main = do
  let blah :: Maybe Value
      blah = decodeStrict sectionJson
  print blah
