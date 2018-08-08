module Main where

import           ClassyPrelude
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           System.Directory           (doesFileExist, removeFile)

import           Conscient.Routes

main :: IO ()
main = do
  let relativePath = "resources/swagger.json"
  specsExist <- doesFileExist relativePath
  when specsExist $ removeFile relativePath
  LBS8.writeFile relativePath $ encodePretty routesSpecification

